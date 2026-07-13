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
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Dynamic (Dynamic, fromDynamic)
import qualified Data.Map.Strict as Map
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple.Types as PG
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
import System.Log.FastLogger (toLogStr)
import qualified Data.Vault.Lazy as Vault
import qualified Network.WebSockets as Websocket
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai
import IHP.RequestVault (pgListenerVaultKey)
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Environment (Environment(..))
import IHP.QueryBuilder.Types (Condition(..), ConditionValue(..), FilterOperator(..), getParamPrinterText)

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

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?request :: Request
    , ?respond :: Respond
    ) => ((?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => IO ResponseReceived) -> IO ResponseReceived
autoRefresh runAction = do
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
                    let placeholderSession =
                            AutoRefreshSession
                                { id
                                , renderView
                                , event
                                , tables = mempty
                                , lastResponse = ""
                                , lastPing
                                , trackedIds = mempty
                                , trackedConditions = mempty
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
                            trackedIds <- readIORef ?trackedIds
                            trackedConditions <- readIORef ?trackedConditions
                            lastPing <- getCurrentTime
                            case capturedResponse of
                                Just lastResponse -> do
                                    updateSession autoRefreshServer id (\session -> session { tables, lastResponse, lastPing, trackedIds, trackedConditions })
                                    totalSessions <- length . (.sessions) <$> readIORef autoRefreshServer
                                    ?request.frameworkConfig.logger (toLogStr ("AutoRefresh register session=" <> tshow id <> " path=" <> cs ?request.rawPathInfo <> " totalSessions=" <> tshow totalSessions))
                                    async (gcSessions autoRefreshServer)
                                    registerNotificationTrigger ?touchedTables autoRefreshServer
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
        unless (sessionId `elem` availableSessions) do
            Websocket.sendClose ?connection ("Auto refresh session unavailable" :: Text)

        when (sessionId `elem` availableSessions) do
            setState AutoRefreshActive { sessionId }
            AutoRefreshSession { renderView, event } <- getSessionById autoRefreshServer sessionId

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

            pure ()

        -- Keep the connection open until it's killed and the onClose is called
        forever receiveDataMessage

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        autoRefreshServer <- getOrCreateAutoRefreshServer
        updateSession autoRefreshServer sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                autoRefreshServer <- getOrCreateAutoRefreshServer
                modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\AutoRefreshSession { id } -> id /= sessionId) server.sessions })
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

    let subscriptionRequired = touchedTables |> filter (`Set.notMember` subscribedTables)
    -- In development, always re-run trigger SQL for all touched tables because
    -- `make db` drops and recreates the database, destroying triggers that were
    -- previously installed. The trigger SQL is idempotent so re-running is safe.
    -- In production, only install triggers for newly seen tables.
    let isDevelopment = ?request.frameworkConfig.environment == Development

    modifyIORef' autoRefreshServer (\server -> server { subscribedTables = server.subscribedTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- subscriptionRequired |> mapM (\table -> do
        withRowLevelSecurityDisabled do
            let pool = ?modelContext.hasqlPool
            runSessionHasql pool (mapM_ HasqlSession.script (notificationTriggerStatements table))

        pgListener |> PGListener.subscribeJSON (channelName table) (\payload -> do
            resolvedPayload <- resolveAutoRefreshPayload payload
            sessions <- (.sessions) <$> readIORef autoRefreshServer
            mapM_ (handleSmartRowChange table resolvedPayload) sessions))

    -- Re-run trigger SQL for already-subscribed tables in dev mode
    when isDevelopment do
        let alreadySubscribed = touchedTables |> filter (`Set.member` subscribedTables)
        forM_ alreadySubscribed \table -> do
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (mapM_ HasqlSession.script (notificationTriggerStatements table))

    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()
  where
    handleSmartRowChange table resolvedPayload AutoRefreshSession { tables, event, trackedIds, trackedConditions }
        | table `Set.member` tables = do
            let conditions = Map.lookup table trackedConditions
                shouldRefreshNow = case Map.lookup table trackedIds of
                    Nothing -> True
                    Just ids | Set.null ids -> True
                    Just ids -> case resolvedPayload of
                        Nothing -> True
                        Just payload -> shouldRefreshForPayload ids conditions payload
            when shouldRefreshNow do
                _ <- MVar.tryPutMVar event ()
                pure ()
        | otherwise = pure ()

shouldRefreshForPayload :: Set Text -> Maybe [Maybe Dynamic] -> AutoRefreshRowChangePayload -> Bool
shouldRefreshForPayload trackedIds maybeConditions payload =
    case payload.payloadOperation of
        AutoRefreshInsert -> case maybeConditions of
            Nothing -> True
            Just conditions -> any (matchesInsertPayloadDynamic newRow) conditions
          where
            newRow = case payload.payloadNewRow of
                Just (Aeson.Object object) -> object
                _ -> AesonKeyMap.empty
        _ -> case extractRowId payload of
            Nothing -> True
            Just rowId -> rowId `Set.member` trackedIds

extractRowId :: AutoRefreshRowChangePayload -> Maybe Text
extractRowId payload =
    (payload.payloadNewRow <|> payload.payloadOldRow) >>= \case
        Aeson.Object object -> case AesonKeyMap.lookup "id" object of
            Just (Aeson.String value) -> Just value
            Just (Aeson.Number value) -> Just (tshow (round value :: Integer))
            _ -> Nothing
        _ -> Nothing

matchesInsertPayloadDynamic :: AesonKeyMap.KeyMap Aeson.Value -> Maybe Dynamic -> Bool
matchesInsertPayloadDynamic _ Nothing = True
matchesInsertPayloadDynamic newRow (Just conditionDynamic) =
    case fromDynamic conditionDynamic of
        Nothing -> True
        Just condition -> matchesInsertPayload condition newRow

matchesInsertPayload :: Condition -> AesonKeyMap.KeyMap Aeson.Value -> Bool
matchesInsertPayload (AndCondition left right) row = matchesInsertPayload left row && matchesInsertPayload right row
matchesInsertPayload (OrCondition left right) row = matchesInsertPayload left row || matchesInsertPayload right row
matchesInsertPayload (ColumnCondition column operator value applyLeft applyRight) row
    | isJust applyLeft || isJust applyRight = True
    | otherwise = case operator of
        EqOp -> matchEq column value row
        IsOp -> matchIs column value row
        _ -> True

matchEq :: Text -> ConditionValue -> AesonKeyMap.KeyMap Aeson.Value -> Bool
matchEq column (Param params) row = case getParamPrinterText params of
    [filterText] -> jsonValueMatchesText (lookupColumn column row) filterText
    _ -> True
matchEq column (Literal text) row = jsonValueMatchesText (lookupColumn column row) text

matchIs :: Text -> ConditionValue -> AesonKeyMap.KeyMap Aeson.Value -> Bool
matchIs column (Literal text) row
    | Text.toLower text == "null" = case lookupColumn column row of
        Nothing -> True
        Just Aeson.Null -> True
        Just _ -> False
    | otherwise = True
matchIs _ (Param _) _ = True

lookupColumn :: Text -> AesonKeyMap.KeyMap Aeson.Value -> Maybe Aeson.Value
lookupColumn column row =
    let columnName = case Text.breakOnEnd "." column of
            ("", value) -> value
            (_, value) -> value
     in AesonKeyMap.lookup (AesonKey.fromText columnName) row

jsonValueMatchesText :: Maybe Aeson.Value -> Text -> Bool
jsonValueMatchesText Nothing _ = True
jsonValueMatchesText (Just jsonValue) filterText = case jsonValue of
    Aeson.String value -> value == unquote filterText
    Aeson.Number value -> tshow (round value :: Integer) == filterText || tshow value == filterText
    Aeson.Bool value -> (if value then "true" else "false") == Text.toLower filterText || (if value then "t" else "f") == Text.toLower filterText
    Aeson.Null -> Text.toLower filterText == "null"
    _ -> True
  where
    unquote value
        | Text.length value >= 2
        , Text.head value == '"'
        , Text.last value == '"' = Text.init (Text.tail value)
        | otherwise = value

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
        |> find (\AutoRefreshSession { id } -> id == sessionId)
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
isSessionExpired now AutoRefreshSession { lastPing } = (now `diffUTCTime` lastPing) > (secondsToNominalDiffTime 60)

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: Text -> ByteString
channelName tableName = "ar_did_change_row_" <> cs tableName

notificationTriggerStatements :: Text -> [Text]
notificationTriggerStatements tableName =
    [ "BEGIN"
    , "CREATE UNLOGGED TABLE IF NOT EXISTS ihp_runtime.large_pg_notifications ("
        <> "id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL, "
        <> "payload TEXT DEFAULT NULL, "
        <> "created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL"
        <> ")"
    , "CREATE INDEX IF NOT EXISTS large_pg_notifications_created_at_index ON ihp_runtime.large_pg_notifications (created_at)"
    , "CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$"
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
        <> "    PERFORM pg_notify('" <> cs (channelName tableName) <> "', payload);\n"
        <> "    IF (TG_OP = 'DELETE') THEN RETURN OLD; ELSE RETURN NEW; END IF;\n"
        <> "END;\n"
        <> "$$ language plpgsql"
    , "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName
    , "CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "()"
    , "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName
    , "CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "()"
    , "DROP TRIGGER IF EXISTS " <> deleteTriggerName <> " ON " <> tableName
    , "CREATE TRIGGER " <> deleteTriggerName <> " AFTER DELETE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "()"
    , "COMMIT"
    ]
  where
    functionName = "ihp_runtime.ar_notify_row_change_" <> tableName
    insertTriggerName = "ar_did_insert_row_" <> tableName
    updateTriggerName = "ar_did_update_row_" <> tableName
    deleteTriggerName = "ar_did_delete_row_" <> tableName

resolveAutoRefreshPayload :: (?modelContext :: ModelContext) => AutoRefreshRowChangePayload -> IO (Maybe AutoRefreshRowChangePayload)
resolveAutoRefreshPayload payload = case payload.payloadLargePayloadId of
    Nothing -> pure (Just payload)
    Just payloadId -> fetchAutoRefreshPayload payloadId

fetchAutoRefreshPayload :: (?modelContext :: ModelContext) => UUID.UUID -> IO (Maybe AutoRefreshRowChangePayload)
fetchAutoRefreshPayload payloadId = do
    payloadResult <- Exception.try (sqlQueryScalar "SELECT payload FROM ihp_runtime.large_pg_notifications WHERE id = ? LIMIT 1" (PG.Only payloadId) :: IO ByteString)
    case payloadResult of
        Left (_ :: Exception.SomeException) -> pure Nothing
        Right payload -> case Aeson.eitherDecodeStrict' payload of
            Left _ -> pure Nothing
            Right result -> pure (Just result)

autoRefreshStateVaultKey :: Vault.Key AutoRefreshState
autoRefreshStateVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshStateVaultKey #-}
