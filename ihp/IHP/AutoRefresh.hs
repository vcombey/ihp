{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.ControllerSupport hiding (request)
import qualified Data.Aeson as Aeson
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Set as Set
import IHP.ModelSupport
import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextEncodingError
import IHP.WebSocket
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import qualified IHP.PGListener as PGListener
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as HasqlStatement
import qualified Hasql.Encoders as HasqlEncoders
import qualified Hasql.Decoders as HasqlDecoders
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

data AutoRefreshOptions = AutoRefreshOptions
    { shouldRefresh :: AutoRefreshChangeSet -> IO Bool
    }

data AutoRefreshConfig
    = AutoRefreshStatementConfig
    | AutoRefreshRowConfig AutoRefreshOptions

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?respond :: Respond
    ) => ((?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => IO ResponseReceived) -> IO ResponseReceived
autoRefresh = autoRefreshInternal AutoRefreshStatementConfig

-- | Like 'autoRefresh', but receives the changed rows and only re-renders when
-- 'shouldRefresh' returns 'True'. This is useful for high-churn tables where
-- most writes cannot affect the current page.
autoRefreshWith :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?respond :: Respond
    ) => AutoRefreshOptions -> ((?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => IO ResponseReceived) -> IO ResponseReceived
autoRefreshWith options = autoRefreshInternal (AutoRefreshRowConfig options)

autoRefreshInternal :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?respond :: Respond
    ) => AutoRefreshConfig -> ((?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => IO ResponseReceived) -> IO ResponseReceived
autoRefreshInternal config runAction = do
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
                            earlyReturnMiddleware (\_ respond -> do
                                let ?request = originalRequest
                                let ?context = ?request
                                let ?respond = respond
                                runControllerAction (action ?theAction)
                                ) waiRequest waiRespond

                    -- Pre-register the session before sending the response so the browser websocket
                    -- can authenticate immediately when the fragment/page HTML arrives.
                    event <- MVar.newEmptyMVar
                    lastPing <- getCurrentTime
                    placeholderSession <- case config of
                        AutoRefreshStatementConfig ->
                            pure AutoRefreshSession
                                { id
                                , renderView
                                , event
                                , tables = mempty
                                , lastResponse = ""
                                , lastPing
                                }
                        AutoRefreshRowConfig options -> do
                            pendingChanges <- newIORef (Just mempty)
                            pure AutoRefreshSessionWithChanges
                                { id
                                , renderView
                                , event
                                , tables = mempty
                                , lastResponse = ""
                                , lastPing
                                , pendingChanges
                                , shouldRefresh = options.shouldRefresh
                                }
                    modifyIORef' autoRefreshServer (\server -> server { sessions = placeholderSession : server.sessions })
                    let removeSession = modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\session -> session.id /= id) server.sessions })

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
                                    updateSession autoRefreshServer id (\session -> session { tables, lastResponse, lastPing })
                                    totalSessions <- length . (.sessions) <$> readIORef autoRefreshServer
                                    ?request.frameworkConfig.logger (toLogStr ("AutoRefresh register session=" <> tshow id <> " path=" <> cs ?request.rawPathInfo <> " totalSessions=" <> tshow totalSessions))
                                    async (gcSessions autoRefreshServer)
                                    case config of
                                        AutoRefreshStatementConfig -> registerNotificationTrigger ?touchedTables autoRefreshServer
                                        AutoRefreshRowConfig {} -> registerRowNotificationTrigger ?touchedTables autoRefreshServer
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

                let handleOtherException :: SomeException -> IO ()
                    handleOtherException ex = ?context.frameworkConfig.logger (toLogStr ("AutoRefresh: Failed to re-render view: " <> tshow ex))

                let onRender = do
                        let currentRequest = ?request
                        (_, capturedResponse) <-
                            captureResponseBody
                                (\_ -> pure (error "AutoRefresh: ResponseReceived placeholder"))
                                (\respond -> session.renderView currentRequest respond)
                        case capturedResponse of
                            Just html -> do
                                responseChanged <- sessionResponseHasChanged autoRefreshServer sessionId html
                                when responseChanged do
                                    sendTextData html
                                    updateSession autoRefreshServer sessionId (\currentSession -> currentSession { lastResponse = html })
                            Nothing -> pure ()

                case session of
                    AutoRefreshSession { event } ->
                        async $ forever do
                            MVar.takeMVar event
                            onRender `catch` handleOtherException
                    AutoRefreshSessionWithChanges { event, pendingChanges, shouldRefresh } ->
                        async $ forever do
                            MVar.takeMVar event
                            pending <- atomicModifyIORef' pendingChanges (\current -> (Just mempty, current))
                            (case pending of
                                Nothing -> onRender
                                Just changes -> do
                                    shouldRender <- shouldRefresh changes
                                    when shouldRender onRender
                                ) `catch` handleOtherException

                -- Keep the connection open until it's killed and the onClose is called.
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
                modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\session -> session.id /= sessionId) server.sessions })
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

registerNotificationTrigger :: (?modelContext :: ModelContext, ?request :: Request) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedTables <- (.subscribedTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)
    -- In development, always re-run trigger SQL for all touched tables because
    -- `make db` drops and recreates the database, destroying triggers that were
    -- previously installed. The trigger SQL is idempotent so re-running is safe.
    -- In production, only install triggers for newly seen tables.
    let isDevelopment = ?request.frameworkConfig.environment == Development

    modifyIORef' autoRefreshServer (\server -> server { subscribedTables = server.subscribedTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- subscriptionRequired |> mapM (\table -> do
        -- We need to add the trigger from the main IHP database role other we will get this error:
        -- ERROR:  permission denied for schema public
        withRowLevelSecurityDisabled do
            let pool = ?modelContext.hasqlPool
            runSessionHasql pool (HasqlSession.script (notificationTriggerSQL table))

        pgListener |> PGListener.subscribe (channelName table) \notification -> do
                sessions <- (.sessions) <$> readIORef autoRefreshServer
                sessions
                    |> mapMaybe (\case
                        AutoRefreshSession { tables, event } | table `Set.member` tables -> Just event
                        _ -> Nothing)
                    |> mapM (\event -> MVar.tryPutMVar event ())
                pure ())

    -- Re-run trigger SQL for already-subscribed tables in dev mode
    when isDevelopment do
        let alreadySubscribed = touchedTables |> filter (\table -> subscribedTables |> Set.member table)
        forM_ alreadySubscribed \table -> do
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (HasqlSession.script (notificationTriggerSQL table))

    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()

registerRowNotificationTrigger :: (?modelContext :: ModelContext, ?request :: Request) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerRowNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedRowTables <- (.subscribedRowTables) <$> readIORef autoRefreshServer
    let subscriptionRequired = filter (`Set.notMember` subscribedRowTables) touchedTables
    let isDevelopment = ?request.frameworkConfig.environment == Development

    modifyIORef' autoRefreshServer (\server -> server { subscribedRowTables = server.subscribedRowTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- forM subscriptionRequired \table -> do
        withRowLevelSecurityDisabled do
            let pool = ?modelContext.hasqlPool
            runSessionHasql pool (mapM_ HasqlSession.script (notificationRowTriggerStatements table))

        pgListener |> PGListener.subscribeJSON (rowChannelName table) (\payload -> do
            resolvedPayload <- resolveAutoRefreshPayload payload
            sessions <- (.sessions) <$> readIORef autoRefreshServer
            mapM_ (handleRowChange table resolvedPayload) sessions)

    when isDevelopment do
        let alreadySubscribed = filter (`Set.member` subscribedRowTables) touchedTables
        forM_ alreadySubscribed \table ->
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (mapM_ HasqlSession.script (notificationRowTriggerStatements table))

    modifyIORef' autoRefreshServer (\server -> server { subscriptions = server.subscriptions <> subscriptions })
  where
    handleRowChange table resolvedPayload = \case
        AutoRefreshSessionWithChanges { tables, pendingChanges, event }
            | table `Set.member` tables -> do
                case resolvedPayload of
                    Nothing -> writeIORef pendingChanges Nothing
                    Just payload ->
                        modifyIORef' pendingChanges (\case
                            Nothing -> Nothing
                            Just current -> Just (insertRowChangeFromPayload table payload current))
                _ <- MVar.tryPutMVar event ()
                pure ()
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
            [cookieText, headerText, queryText]
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
    modifyIORef' server (\server -> server { sessions = map updateSession' server.sessions })
    pure ()

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
    modifyIORef' autoRefreshServer (\autoRefreshServer -> autoRefreshServer { sessions = filter (not . isSessionExpired now) autoRefreshServer.sessions })

-- | A session is expired if it was not pinged in the last 60 seconds
isSessionExpired :: UTCTime -> AutoRefreshSession -> Bool
isSessionExpired now session = (now `diffUTCTime` session.lastPing) > (secondsToNominalDiffTime 60)

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: Text -> ByteString
channelName tableName = "ar_did_change_" <> cs tableName

-- | Returns a SQL script to set up database notification triggers.
--
-- Wrapped in a DO $$ block with EXCEPTION handler because concurrent requests
-- can race to CREATE the same function or trigger, causing PostgreSQL to throw
-- 'tuple concurrently updated' (SQLSTATE XX000), 'duplicate_object' (42710),
-- or 'duplicate_function' (42723). This is safe to ignore: the
-- other connection's CREATE will have succeeded.
notificationTriggerSQL :: Text -> Text
notificationTriggerSQL tableName =
        "DO $$\n"
        <> "BEGIN\n"
        <> "    IF to_regprocedure('" <> functionName <> "()') IS NULL THEN\n"
        <> "        CREATE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $BODY$"
            <> "BEGIN\n"
            <> "    PERFORM pg_notify('" <> cs (channelName tableName) <> "', '');\n"
            <> "    RETURN new;\n"
            <> "END;\n"
            <> "$BODY$ language plpgsql;\n"
        <> "    END IF;\n"
        <> "    IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = '" <> insertTriggerName <> "' AND tgrelid = '" <> tableName <> "'::regclass) THEN\n"
        <> "        CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    END IF;\n"
        <> "    IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = '" <> updateTriggerName <> "' AND tgrelid = '" <> tableName <> "'::regclass) THEN\n"
        <> "        CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    END IF;\n"
        <> "    IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = '" <> deleteTriggerName <> "' AND tgrelid = '" <> tableName <> "'::regclass) THEN\n"
        <> "        CREATE TRIGGER " <> deleteTriggerName <> " AFTER DELETE ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    END IF;\n"
        <> "EXCEPTION\n"
        <> "    WHEN SQLSTATE 'XX000' THEN null; -- 'tuple concurrently updated': another connection installed it first\n"
        <> "    WHEN SQLSTATE '42710' THEN null; -- 'duplicate_object': another connection installed it first\n"
        <> "    WHEN SQLSTATE '42723' THEN null; -- 'duplicate_function': another connection installed it first\n"
        <> "END; $$"
    where
        functionName = "ihp_runtime.ar_notify_did_change_" <> tableName
        insertTriggerName = "ar_did_insert_" <> tableName
        updateTriggerName = "ar_did_update_" <> tableName
        deleteTriggerName = "ar_did_delete_" <> tableName

notificationRowTriggerStatements :: Text -> [Text]
notificationRowTriggerStatements tableName =
    [ "CREATE UNLOGGED TABLE IF NOT EXISTS ihp_runtime.large_pg_notifications ("
        <> "id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL, "
        <> "payload TEXT DEFAULT NULL, "
        <> "created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL"
        <> ")"
    , "CREATE INDEX IF NOT EXISTS large_pg_notifications_created_at_index ON ihp_runtime.large_pg_notifications (created_at)"
    , "DO $$\n"
        <> "BEGIN\n"
        <> "    IF to_regprocedure('" <> functionName <> "()') IS NULL THEN\n"
        <> "        CREATE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $BODY$\n"
        <> "DECLARE\n"
        <> "    payload TEXT;\n"
        <> "    large_pg_notification_id UUID;\n"
        <> "BEGIN\n"
        <> "    IF (TG_OP = 'DELETE') THEN\n"
        <> "        payload := jsonb_build_object('op', lower(TG_OP), 'old', to_jsonb(OLD))::text;\n"
        <> "    ELSIF (TG_OP = 'UPDATE') THEN\n"
        <> "        payload := jsonb_build_object('op', lower(TG_OP), 'old', to_jsonb(OLD), 'new', to_jsonb(NEW))::text;\n"
        <> "    ELSE\n"
        <> "        payload := jsonb_build_object('op', lower(TG_OP), 'new', to_jsonb(NEW))::text;\n"
        <> "    END IF;\n"
        <> "    IF octet_length(payload) > 7800 THEN\n"
        <> "        INSERT INTO ihp_runtime.large_pg_notifications (payload) VALUES (payload) RETURNING id INTO large_pg_notification_id;\n"
        <> "        payload := jsonb_build_object('op', lower(TG_OP), 'payloadId', large_pg_notification_id::text)::text;\n"
        <> "        DELETE FROM ihp_runtime.large_pg_notifications WHERE created_at < CURRENT_TIMESTAMP - interval '30s';\n"
        <> "    END IF;\n"
        <> "    PERFORM pg_notify('" <> cs (rowChannelName tableName) <> "', payload);\n"
        <> "    IF (TG_OP = 'DELETE') THEN RETURN OLD; ELSE RETURN NEW; END IF;\n"
        <> "END;\n"
        <> "$BODY$ language plpgsql;\n"
        <> "    END IF;\n"
        <> "    IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = '" <> insertTriggerName <> "' AND tgrelid = '" <> tableName <> "'::regclass) THEN\n"
        <> "        CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    END IF;\n"
        <> "    IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = '" <> updateTriggerName <> "' AND tgrelid = '" <> tableName <> "'::regclass) THEN\n"
        <> "        CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    END IF;\n"
        <> "    IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = '" <> deleteTriggerName <> "' AND tgrelid = '" <> tableName <> "'::regclass) THEN\n"
        <> "        CREATE TRIGGER " <> deleteTriggerName <> " AFTER DELETE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    END IF;\n"
        <> "EXCEPTION\n"
        <> "    WHEN SQLSTATE 'XX000' THEN null;\n"
        <> "    WHEN SQLSTATE '42710' THEN null;\n"
        <> "    WHEN SQLSTATE '42723' THEN null;\n"
        <> "END; $$"
    ]
  where
    functionName = "ihp_runtime.ar_notify_row_change_" <> tableName
    insertTriggerName = "ar_did_insert_row_" <> tableName
    updateTriggerName = "ar_did_update_row_" <> tableName
    deleteTriggerName = "ar_did_delete_row_" <> tableName

rowChannelName :: Text -> ByteString
rowChannelName tableName = "ar_did_change_row_" <> cs tableName

resolveAutoRefreshPayload :: (?modelContext :: ModelContext) => AutoRefreshRowChangePayload -> IO (Maybe AutoRefreshRowChangePayload)
resolveAutoRefreshPayload payload = case payload.payloadLargePayloadId of
    Nothing -> pure (Just payload)
    Just payloadId -> fetchAutoRefreshPayload payloadId

fetchAutoRefreshPayload :: (?modelContext :: ModelContext) => UUID.UUID -> IO (Maybe AutoRefreshRowChangePayload)
fetchAutoRefreshPayload payloadId = do
    let statement = HasqlStatement.preparable
            "SELECT payload FROM ihp_runtime.large_pg_notifications WHERE id = $1 LIMIT 1"
            (HasqlEncoders.param (HasqlEncoders.nonNullable HasqlEncoders.uuid))
            (HasqlDecoders.singleRow (HasqlDecoders.column (HasqlDecoders.nullable HasqlDecoders.text)))
    result <- Exception.try (sqlStatementHasql ?modelContext.hasqlPool payloadId statement)
    case result of
        Left (_ :: SomeException) -> pure Nothing
        Right Nothing -> pure Nothing
        Right (Just payload) -> case Aeson.eitherDecodeStrict' (Text.encodeUtf8 payload) of
            Left _ -> pure Nothing
            Right decoded -> pure (Just decoded)

autoRefreshStateVaultKey :: Vault.Key AutoRefreshState
autoRefreshStateVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshStateVaultKey #-}
