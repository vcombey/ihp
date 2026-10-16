{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.AutoRefresh.ChangeSet (AutoRefreshChangeSet)
import qualified IHP.AutoRefresh.ChangeSet as ChangeSet
import IHP.ControllerSupport hiding (request)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Set as Set
import qualified Data.TMap as TMap
import IHP.ModelSupport
import qualified Data.Aeson as Aeson
import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (void)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextEncodingError
import IHP.WebSocket
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import qualified IHP.PGListener as PGListener
import qualified Hasql.Session as HasqlSession
import System.Log.FastLogger (toLogStr)
import qualified Data.Vault.Lazy as Vault
import qualified Network.WebSockets as Websocket
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai
import IHP.RequestVault (pgListenerVaultKey)
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Environment (Environment(..))

{-# NOINLINE globalAutoRefreshServerVar #-}
globalAutoRefreshServerVar :: MVar.MVar (Maybe (IORef AutoRefreshServer))
globalAutoRefreshServerVar = unsafePerformIO (MVar.newMVar Nothing)

{-# NOINLINE notificationTriggerRegistrationLock #-}
notificationTriggerRegistrationLock :: MVar.MVar ()
notificationTriggerRegistrationLock = unsafePerformIO (MVar.newMVar ())

getOrCreateAutoRefreshServer :: (?request :: Request) => IO (IORef AutoRefreshServer)
getOrCreateAutoRefreshServer =
    MVar.modifyMVar globalAutoRefreshServerVar $ \case
        Just server -> pure (Just server, server)
        Nothing -> do
            let pgListener = case Vault.lookup pgListenerVaultKey ?request.vault of
                    Just pl -> pl
                    Nothing -> error "getOrCreateAutoRefreshServer: PGListener not found in request vault"
            server <- newIORef (newAutoRefreshServer pgListener)
            pure (Just server, server)

-- | Columns safe to publish in PostgreSQL notifications. Configure once in
-- Config.hs with @option (AutoRefreshPayloadColumns ["id", "organization_id"])@.
-- Missing columns are unknown to the predicate. The default publishes no rows.
newtype AutoRefreshPayloadColumns = AutoRefreshPayloadColumns [Text]

-- | An explicit payload-only invalidation policy. Keep the predicate free of SQL.
data AutoRefreshOptions = AutoRefreshOptions
    { shouldRefresh :: AutoRefreshChangeSet -> IO Bool
    }

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?respond :: Respond
    ) => ((?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => IO ResponseReceived) -> IO ResponseReceived
autoRefresh = autoRefreshInternal Nothing

-- | Restore the explicit filtering API while retaining current session/RLS handling.
autoRefreshWith :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?respond :: Respond
    ) => AutoRefreshOptions -> ((?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => IO ResponseReceived) -> IO ResponseReceived
autoRefreshWith options = autoRefreshInternal (Just options)

autoRefreshInternal :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?respond :: Respond
    ) => Maybe AutoRefreshOptions -> ((?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => IO ResponseReceived) -> IO ResponseReceived
autoRefreshInternal options runAction = do
    -- When PGListener is not available, degrade gracefully to a
    -- plain action without auto-refresh.
    case Vault.lookup pgListenerVaultKey ?request.vault of
        Nothing -> runAction
        Just _ -> do
            let autoRefreshState = Vault.lookup autoRefreshStateVaultKey ?request.vault
            autoRefreshServer <- getOrCreateAutoRefreshServer

            case autoRefreshState of
                Just (AutoRefreshEnabled {}) -> do
                    -- When this function calls the current controller action in the other case
                    -- we will evaluate this branch
                    runAction
                _ -> do
                    availableSessions <- getAvailableSessions autoRefreshServer

                    id <- UUID.nextRandom

                    -- Update the vault with AutoRefreshEnabled so that autoRefreshMeta can read it
                    let newRequest = ?request { vault = Vault.insert autoRefreshStateVaultKey (AutoRefreshEnabled id) ?request.vault }
                    let ?request = newRequest

                    -- Capture the current request and context for re-rendering. The
                    -- request vault carries all per-request state (current user, flash
                    -- messages, framework config, ...) so passing the closure-captured
                    -- values back into the renderView callback is enough.
                    let originalRequest = ?request
                    let renderView = \waiRequest waiRespond -> do
                            withTableReadTracker do
                                result <- earlyReturnMiddleware (\_ respond -> do
                                    let ?request = originalRequest
                                    let ?context = ?request
                                    let ?respond = respond
                                    runControllerAction (action ?theAction)
                                    ) waiRequest waiRespond
                                tables <- readIORef ?touchedTables
                                commitAutoRefreshTables
                                    (registerNotificationTrigger ?touchedTables autoRefreshServer)
                                    autoRefreshServer id tables
                                pure result

                    -- Pre-register the session before sending the response so the browser websocket
                    -- can authenticate immediately when the fragment/page HTML arrives.
                    event <- MVar.newEmptyMVar
                    lastPing <- getCurrentTime
                    let placeholderSession =
                            AutoRefreshSession
                                { id
                                , renderView
                                , event
                                , tables = mempty
                                , lastResponse = ""
                                , lastPing
                                }
                    let initialSession = case options of
                            Nothing -> placeholderSession
                            Just AutoRefreshOptions { shouldRefresh } -> FilteredAutoRefreshSession
                                { id, renderView, event, tables = mempty, lastResponse = "", lastPing, shouldRefresh }
                    atomicModifyIORef' autoRefreshServer (\server -> (server { sessions = initialSession : server.sessions }, ()))
                    let removeSession = atomicModifyIORef' autoRefreshServer (\server -> (server { sessions = filter (\session -> session.id /= id) server.sessions }, ()))

                    -- We save the allowed session ids to the session cookie to only grant a client access
                    -- to sessions it initially opened itself
                    --
                    -- Otherwise you might try to guess session UUIDs to access other peoples auto refresh sessions
                    let serializedAvailableSessions = map UUID.toText (id:availableSessions) |> Text.intercalate ""
                    setSession "autoRefreshSessions" serializedAvailableSessions

                    withTableReadTracker
                        (do
                            (result, capturedResponse) <- captureResponseBody ?respond \respond -> do
                                let ?respond = respond
                                runAction

                            -- After the action completes, set up the auto refresh session
                            tables <- readIORef ?touchedTables
                            lastPing <- getCurrentTime
                            case capturedResponse of
                                Just lastResponse -> do
                                    registerNotificationTrigger ?touchedTables autoRefreshServer
                                    updateSession autoRefreshServer id (\session -> session { tables, lastResponse, lastPing })
                                    totalSessions <- length . (.sessions) <$> readIORef autoRefreshServer
                                    ?request.frameworkConfig.logger (toLogStr ("AutoRefresh register session=" <> tshow id <> " path=" <> cs ?request.rawPathInfo <> " totalSessions=" <> tshow totalSessions))
                                    void (async (gcSessions autoRefreshServer))
                                Nothing -> removeSession

                            pure result
                        )
                        `Exception.onException` removeSession

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        let ?context = ?request
        sessionId <- receiveData @UUID

        autoRefreshServer <- getOrCreateAutoRefreshServer
        availableSessions <- getAvailableSessions autoRefreshServer
        ?context.frameworkConfig.logger . toLogStr $
            "AutoRefresh websocket session="
                <> tshow sessionId
                <> " available="
                <> tshow (sessionId `elem` availableSessions)
        if sessionId `elem` availableSessions
            then do
                setState AutoRefreshActive { sessionId }
                session <- getSessionById autoRefreshServer sessionId
                let renderView = session.renderView
                let event = session.event

                let handleOtherException :: SomeException -> IO ()
                    handleOtherException ex = ?context.frameworkConfig.logger (toLogStr ("AutoRefresh: Failed to re-render view: " <> tshow ex))

                async $ forever do
                    MVar.takeMVar event
                    let currentRequest = ?request
                    (do
                        (_, capturedResponse) <- captureResponseBody (\_ -> pure (error "AutoRefresh: ResponseReceived placeholder")) \respond ->
                            renderView currentRequest respond
                        case capturedResponse of
                            Just html -> do
                                responseChanged <- sessionResponseHasChanged autoRefreshServer sessionId html
                                when responseChanged do
                                    sendTextData html
                                    updateSession autoRefreshServer sessionId (\session -> session { lastResponse = html })
                            Nothing -> pure ()
                        ) `catch` handleOtherException
                    pure ()
                -- Keep the connection open until onClose cleans up the active session.
                forever receiveDataMessage
            else
                Websocket.sendClose ?connection ("Auto refresh session unavailable" :: Text)

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        autoRefreshServer <- getOrCreateAutoRefreshServer
        updateSession autoRefreshServer sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                autoRefreshServer <- getOrCreateAutoRefreshServer
                atomicModifyIORef' autoRefreshServer (\server -> (server { sessions = filter (\session -> session.id /= sessionId) server.sessions }, ()))
            AwaitingSessionID -> pure ()


-- | Runs an action while capturing the response body.
-- Returns the action's result and the captured body (if it was a ResponseBuilder).
-- Only captures ResponseBuilder responses (used by HSX/Blaze rendering).
captureResponseBody :: Respond -> (Respond -> IO a) -> IO (a, Maybe LByteString)
captureResponseBody originalRespond action = do
    bodyRef <- newIORef Nothing
    let capturingRespond response = do
            case response of
                Wai.ResponseBuilder _status _headers builder -> do
                    let body = ByteString.toLazyByteString builder
                    evaluatedBody <- Exception.evaluate body
                    writeIORef bodyRef (Just evaluatedBody)
                _ -> pure ()
            originalRespond response
    result <- action capturingRespond
    captured <- readIORef bodyRef
    pure (result, captured)

-- | Install new dependencies before publishing them. A failed installation
-- leaves the session watching the tables of its last successful render.
commitAutoRefreshTables :: IO () -> IORef AutoRefreshServer -> UUID -> Set Text -> IO ()
commitAutoRefreshTables registerTables server sessionId tables = do
    registerTables
    updateSession server sessionId (\session -> session { tables })

registerNotificationTrigger :: (?modelContext :: ModelContext, ?request :: Request) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer =
    MVar.withMVar notificationTriggerRegistrationLock \_ -> do
        touchedTables <- Set.toList <$> readIORef touchedTablesVar
        subscribedTables <- (.subscribedTables) <$> readIORef autoRefreshServer

        let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)

        -- In development, always re-run trigger SQL for all touched tables because
        -- `make db` drops and recreates the database, destroying triggers that were
        -- previously installed. The trigger SQL is idempotent so re-running is safe.
        -- In production, only install triggers for newly seen tables.
        let isDevelopment = ?request.frameworkConfig.environment == Development
        let AutoRefreshPayloadColumns payloadColumns = fromMaybe (AutoRefreshPayloadColumns [])
                (TMap.lookup ?request.frameworkConfig.appConfig)
        let triggerSQL = notificationTriggerSQLWithColumns payloadColumns

        pgListener <- (.pgListener) <$> readIORef autoRefreshServer
        forM_ subscriptionRequired \table -> do
            -- We need to add the trigger from the main IHP database role other we will get this error:
            -- ERROR:  permission denied for schema public
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (HasqlSession.script (triggerSQL table))

            subscription <- pgListener |> PGListener.subscribe (channelName table) \notification ->
                processAutoRefreshWithPayload autoRefreshServer table notification.notificationData

            atomicModifyIORef' autoRefreshServer \server ->
                ( server
                    { subscriptions = server.subscriptions <> [subscription]
                    , subscribedTables = Set.insert table server.subscribedTables
                    }
                , ()
                )

        -- Re-run trigger SQL for already-subscribed tables in dev mode
        when isDevelopment do
            let alreadySubscribed = touchedTables |> filter (\table -> subscribedTables |> Set.member table)
            forM_ alreadySubscribed \table -> do
                withRowLevelSecurityDisabled do
                    let pool = ?modelContext.hasqlPool
                    runSessionHasql pool (HasqlSession.script (triggerSQL table))

-- | Dispatch notifications without executing SQL. Invalid/oversized
-- payloads and predicate exceptions refresh conservatively instead of reading SQL.
processAutoRefreshWithPayload :: IORef AutoRefreshServer -> Text -> ByteString -> IO ()
processAutoRefreshWithPayload server table payload = do
    sessions <- (.sessions) <$> readIORef server
    let changes = ChangeSet.decodeChangeSet table payload
    forM_ sessions \case
        FilteredAutoRefreshSession { tables, event, shouldRefresh }
            | table `Set.member` tables -> do
                refresh <- case changes of
                    Nothing -> pure True
                    Just value -> shouldRefresh value `Exception.catch` \(exception :: SomeException) ->
                        case Exception.fromException exception :: Maybe Exception.SomeAsyncException of
                            Just asyncException -> Exception.throwIO asyncException
                            Nothing -> pure True
                when refresh (void (MVar.tryPutMVar event ()))
        AutoRefreshSession { tables, event }
            | table `Set.member` tables -> void (MVar.tryPutMVar event ())
        _ -> pure ()

-- | Returns the ids of all sessions available to the client based on what sessions are found in the session cookie
getAvailableSessions :: (?request :: Request) => IORef AutoRefreshServer -> IO [UUID]
getAvailableSessions autoRefreshServer = do
    allSessions <- (.sessions) <$> readIORef autoRefreshServer
    cookieText <- fromMaybe "" <$> getSession "autoRefreshSessions"
    let headerText = getClientAutoRefreshSessionsHeader ?request
    let queryText = getClientAutoRefreshSessionsQuery ?request
    let uuidCharCount = Text.length (UUID.toText UUID.nil)
    let allSessionIds = map (.id) allSessions
    let requestedSessionIds =
            -- Explicit client state replaces the older cookie snapshot. Discarded
            -- fragments can remain on the server until GC, but must not grow the cookie.
            (if Text.null headerText && Text.null queryText
                then [cookieText]
                else [headerText, queryText])
                |> map (parseSessionIds uuidCharCount)
                |> concat
                |> List.nub
    requestedSessionIds
        |> filter (\id -> id `elem` allSessionIds)
        |> pure

getClientAutoRefreshSessionsHeader :: Request -> Text
getClientAutoRefreshSessionsHeader request =
    request.requestHeaders
        |> lookup "X-IHP-Auto-Refresh-Sessions"
        |> fmap (Text.decodeUtf8With TextEncodingError.lenientDecode)
        |> fromMaybe ""

getClientAutoRefreshSessionsQuery :: Request -> Text
getClientAutoRefreshSessionsQuery request =
    request.queryString
        |> lookup "autoRefreshSessions"
        |> join
        |> fmap (Text.decodeUtf8With TextEncodingError.lenientDecode)
        |> fromMaybe ""

parseSessionIds :: Int -> Text -> [UUID]
parseSessionIds uuidCharCount text =
    text
        |> Text.chunksOf uuidCharCount
        |> mapMaybe UUID.fromText

-- | Returns a session for a given session id. Errors in case the session does not exist.
getSessionById :: IORef AutoRefreshServer -> UUID -> IO AutoRefreshSession
getSessionById autoRefreshServer sessionId = do
    autoRefreshServer <- readIORef autoRefreshServer
    autoRefreshServer.sessions
        |> find (\session -> session.id == sessionId)
        |> Maybe.fromMaybe (error "getSessionById: Could not find the session")
        |> pure

-- | Applies a update function to a session specified by its session id
updateSession :: IORef AutoRefreshServer -> UUID -> (AutoRefreshSession -> AutoRefreshSession) -> IO ()
updateSession server sessionId updateFunction = do
    let updateSession' session = if session.id == sessionId then updateFunction session else session
    atomicModifyIORef' server (\server -> (server { sessions = map updateSession' server.sessions }, ()))

-- | Returns 'True' when the rendered html differs from the session's latest
-- known response.
--
-- This must read the current session state instead of comparing against a
-- websocket-local snapshot, otherwise switching back to an earlier DOM state
-- can be incorrectly suppressed as "unchanged".
sessionResponseHasChanged :: IORef AutoRefreshServer -> UUID -> LByteString -> IO Bool
sessionResponseHasChanged autoRefreshServer sessionId html = do
    currentLastResponse <- (.lastResponse) <$> getSessionById autoRefreshServer sessionId
    pure (html /= currentLastResponse)

-- | Removes all expired sessions
--
-- This is useful to avoid dead sessions hanging around. This can happen when a websocket connection was never established
-- after the initial request. Then the onClose of the websocket app is never called and thus the session will not be
-- removed automatically.
gcSessions :: IORef AutoRefreshServer -> IO ()
gcSessions autoRefreshServer = do
    now <- getCurrentTime
    atomicModifyIORef' autoRefreshServer (\server -> (server { sessions = filter (not . isSessionExpired now) server.sessions }, ()))

-- | A session is expired if it was not pinged in the last 60 seconds
isSessionExpired :: UTCTime -> AutoRefreshSession -> Bool
isSessionExpired now session = (now `diffUTCTime` session.lastPing) > (secondsToNominalDiffTime 60)

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: Text -> ByteString
channelName tableName = "ar_did_change_" <> cs tableName

-- | Install row notifications for tables, or conservative statement notifications
-- on the underlying tables of a view (including nested and private-schema views).
--
-- Serialize installation per notification function to avoid concurrent catalog
-- inserts and updates. Retain the exception handler for older concurrent installers.
notificationTriggerSQL :: Text -> Text
notificationTriggerSQL = notificationTriggerSQLWithColumns []

notificationTriggerSQLWithColumns :: [Text] -> Text -> Text
notificationTriggerSQLWithColumns columns tableName =
        "DO $$\n"
        <> "DECLARE source RECORD;\n"
        <> "BEGIN\n"
        <> "    IF EXISTS (SELECT 1 FROM pg_proc WHERE oid = to_regprocedure('" <> functionName <> "()') AND position('" <> sqlString marker <> "' in prosrc) > 0)\n"
        <> "       AND (SELECT count(*) FROM pg_trigger WHERE tgrelid = '" <> tableName <> "'::regclass\n"
        <> "            AND tgname IN ('" <> insertTriggerName <> "', '" <> updateTriggerName <> "', '" <> deleteTriggerName <> "')\n"
        <> "            AND (tgtype & 1) = 1 AND tgfoid = to_regprocedure('" <> functionName <> "()')) = 3\n"
        <> "       AND EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid = '" <> tableName <> "'::regclass AND tgname = '" <> truncateTriggerName <> "') THEN\n"
        <> "        RETURN;\n"
        <> "    END IF;\n"
        <> "    PERFORM pg_advisory_xact_lock(hashtextextended('" <> functionName <> "', 0));\n"
        <> "    CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $BODY$"
            <> "DECLARE\n"
            <> "    payload TEXT;\n"
            <> "    old_row JSONB; new_row JSONB;\n"
            <> "BEGIN\n"
            <> "    IF TG_LEVEL = 'STATEMENT' THEN\n"
            <> "        PERFORM pg_notify('" <> cs (channelName tableName) <> "', jsonb_build_object('op', lower(TG_OP))::text);\n"
            <> "        RETURN NULL;\n"
            <> "    END IF;\n"
            <> "    -- " <> marker <> "\n"
            <> "    old_row := CASE WHEN TG_OP = 'INSERT' THEN NULL ELSE to_jsonb(OLD) END;\n"
            <> "    new_row := CASE WHEN TG_OP = 'DELETE' THEN NULL ELSE to_jsonb(NEW) END;\n"
            <> "    payload := jsonb_build_object('op', lower(TG_OP), 'id', CASE WHEN TG_OP = 'DELETE' THEN old_row->>'id' ELSE new_row->>'id' END, 'old', " <> projectRow "old_row" <> ", 'new', " <> projectRow "new_row" <> ")::text;\n"
            <> "    IF octet_length(payload) > 7000 THEN\n"
            <> "        payload := jsonb_build_object('op', lower(TG_OP), 'id', CASE WHEN TG_OP = 'DELETE' THEN old_row->>'id' ELSE new_row->>'id' END)::text;\n"
            <> "    END IF;\n"
            <> "    PERFORM pg_notify('" <> cs (channelName tableName) <> "', payload);\n"
            <> "    RETURN NULL;\n"
            <> "END;\n"
            <> "$BODY$ language plpgsql;\n"
        <> "    IF EXISTS (SELECT 1 FROM pg_class WHERE oid = '" <> tableName <> "'::regclass AND relkind = 'v') THEN\n"
        <> "        FOR source IN WITH RECURSIVE dependencies(oid) AS (\n"
        <> "            SELECT '" <> tableName <> "'::regclass::oid\n"
        <> "            UNION SELECT d.refobjid FROM dependencies s\n"
        <> "            JOIN pg_rewrite r ON r.ev_class = s.oid\n"
        <> "            JOIN pg_depend d ON d.classid = 'pg_rewrite'::regclass AND d.objid = r.oid\n"
        <> "            WHERE d.refclassid = 'pg_class'::regclass AND d.refobjid <> s.oid\n"
        <> "        ) SELECT c.oid, n.nspname, c.relname FROM dependencies d\n"
        <> "          JOIN pg_class c ON c.oid = d.oid JOIN pg_namespace n ON n.oid = c.relnamespace\n"
        <> "          WHERE c.relkind IN ('r', 'p') LOOP\n"
        <> "                EXECUTE format('CREATE OR REPLACE TRIGGER %I AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON %I.%I FOR EACH STATEMENT EXECUTE FUNCTION %s', '" <> viewTriggerName <> "', source.nspname, source.relname, '" <> functionName <> "()'::regprocedure);\n"
        <> "        END LOOP;\n"
        <> "        RETURN;\n"
        <> "    END IF;\n"
        <> "    CREATE OR REPLACE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    CREATE OR REPLACE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    CREATE OR REPLACE TRIGGER " <> deleteTriggerName <> " AFTER DELETE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    CREATE OR REPLACE TRIGGER " <> truncateTriggerName <> " AFTER TRUNCATE ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "EXCEPTION\n"
        <> "    WHEN SQLSTATE 'XX000' THEN null; -- 'tuple concurrently updated': another connection installed it first\n"
        <> "END; $$"
    where
        sqlString = Text.replace "'" "''"
        -- Quoted JSON keys, never SQL identifiers. No row content is published
        -- unless its column was explicitly allowed by application configuration.
        projectRow row = if null columns then "NULL" else
            "CASE WHEN " <> row <> " IS NULL THEN NULL ELSE jsonb_strip_nulls(jsonb_build_object("
                <> Text.intercalate ", " ["'" <> sqlString column <> "', " <> row <> "->'" <> sqlString column <> "'" | column <- columns]
                <> ")) END"
        marker = "ar_payload_v3:" <> cs (Aeson.encode columns)
        -- Reuse the existing names so installing the row-level variant also
        -- replaces the legacy statement-level triggers during an upgrade.
        functionName = "ihp_runtime.ar_notify_did_change_" <> tableName
        insertTriggerName = "ar_did_insert_" <> tableName
        updateTriggerName = "ar_did_update_" <> tableName
        deleteTriggerName = "ar_did_delete_" <> tableName
        viewTriggerName = "ar_view_" <> tableName
        truncateTriggerName = "ar_did_truncate_" <> tableName

autoRefreshStateVaultKey :: Vault.Key AutoRefreshState
autoRefreshStateVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshStateVaultKey #-}
