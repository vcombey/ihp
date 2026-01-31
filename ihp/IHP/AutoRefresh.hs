{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.AutoRefresh.View (autoRefreshMeta)
import IHP.ControllerSupport
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import IHP.ModelSupport
import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Text.Lazy as LazyText
import IHP.WebSocket
import IHP.Controller.Context
import IHP.Controller.Response
import qualified IHP.PGListener as PGListener
import qualified Database.PostgreSQL.Simple.Types as PG
import Data.String.Interpolate.IsString
import qualified IHP.Log as Log
import qualified Data.Vault.Lazy as Vault
import Network.HTTP.Types.Header (ResponseHeaders, hContentType)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Timeout as Timeout
import qualified Text.Blaze.Html.Renderer.Text as BlazeText
import Network.Wai

initAutoRefresh :: (?context :: ControllerContext) => IO ()
initAutoRefresh = do
    putContext AutoRefreshDisabled

-- | Limits the client-side morphing to the DOM node matching the given CSS selector.
-- Useful when combining Auto Refresh with fragment based renderers such as HTMX.
setAutoRefreshTarget :: (?context :: ControllerContext) => Text -> IO ()
setAutoRefreshTarget selector = putContext (AutoRefreshTarget selector)

data AutoRefreshOptions action = AutoRefreshOptions
    { shouldRefresh :: action -> AutoRefreshChangeSet -> IO Bool
    }

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefresh runAction = do
    autoRefreshState <- fromContext @AutoRefreshState
    let autoRefreshServer = autoRefreshServerFromRequest request

    case autoRefreshState of
        AutoRefreshDisabled -> do
            availableSessions <- getAvailableSessions autoRefreshServer

            id <- UUID.nextRandom

            -- We save the current state of the controller context here. This includes e.g. all current
            -- flash messages, the current user, ...
            --
            -- This frozen context is used as a "template" inside renderView to make a new controller context
            -- with the exact same content we had when rendering the initial page, whenever we do a server-side re-rendering
            putContext (AutoRefreshEnabled id)

            -- We save the allowed session ids to the session cookie to only grant a client access
            -- to sessions it initially opened itself
            --
            -- Otherwise you might try to guess session UUIDs to access other peoples auto refresh sessions
            setSession "autoRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")

            frozenControllerContext <- freeze ?context

            let renderView = \waiRequest waiRespond -> do
                    controllerContext <- unfreeze frozenControllerContext
                    let ?context = controllerContext
                    let ?request = waiRequest
                    let ?respond = waiRespond
                    putContext waiRequest
                    putContext (AutoRefreshEnabled id)
                    action ?theAction

            withTableReadTracker do
                let handleResponse exception@(ResponseException response) = case response of
                        Wai.ResponseBuilder status headers builder -> do
                            tables <- readIORef ?touchedTables
                            lastPing <- getCurrentTime

                            -- It's important that we evaluate the response to HNF here
                            -- Otherwise a response `error "fail"` will break auto refresh and cause
                            -- the action to be unreachable until the server is restarted.
                            --
                            -- Specifically a request like this will crash the action:
                            --
                            -- > curl --header 'Accept: application/json' http://localhost:8000/ShowItem?itemId=6bbe1a72-400a-421e-b26a-ff58d17af3e5
                            --
                            -- Let's assume that the view has no implementation for JSON responses. Then
                            -- it will render a 'error "JSON not implemented"'. After this curl request
                            -- all future HTML requests to the current action will fail with a 503.
                            --
                            rawResponse <- Exception.evaluate (ByteString.toLazyByteString builder)
                            lastResponse <- ensureAutoRefreshMeta headers rawResponse
                            let response' =
                                    if lastResponse == rawResponse
                                        then response
                                        else Wai.ResponseBuilder status headers (ByteString.fromLazyByteString lastResponse)

                            event <- MVar.newEmptyMVar
                            let session = AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing }
                            modifyIORef' autoRefreshServer (\s -> s { sessions = session:s.sessions } )
                            async (gcSessions autoRefreshServer)

                            registerNotificationTrigger ?touchedTables autoRefreshServer

                            throw (ResponseException response')
                        _   -> error "Unimplemented WAI response type."

                runAction `Exception.catch` handleResponse
        AutoRefreshEnabled {} -> do
            -- When this function calls the 'action ?theAction' in the other case
            -- we will evaluate this branch
            runAction

autoRefreshWith :: forall action. (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    , ?respond :: Respond
    ) => AutoRefreshOptions action -> ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefreshWith options runAction = do
    autoRefreshState <- fromContext @AutoRefreshState
    let autoRefreshServer = autoRefreshServerFromRequest request

    case autoRefreshState of
        AutoRefreshDisabled -> do
            availableSessions <- getAvailableSessions autoRefreshServer

            id <- UUID.nextRandom

            putContext (AutoRefreshEnabled id)

            setSession "autoRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")

            frozenControllerContext <- freeze ?context

            let renderView = \waiRequest waiRespond -> do
                    controllerContext <- unfreeze frozenControllerContext
                    let ?context = controllerContext
                    let ?request = waiRequest
                    let ?respond = waiRespond
                    putContext waiRequest
                    putContext (AutoRefreshEnabled id)
                    action ?theAction

            let shouldRefresh' = options.shouldRefresh ?theAction

            withTableReadTracker do
                let handleResponse exception@(ResponseException response) = case response of
                        Wai.ResponseBuilder status headers builder -> do
                            tables <- readIORef ?touchedTables
                            lastPing <- getCurrentTime
                            rawResponse <- Exception.evaluate (ByteString.toLazyByteString builder)
                            lastResponse <- ensureAutoRefreshMeta headers rawResponse
                            let response' =
                                    if lastResponse == rawResponse
                                        then response
                                        else Wai.ResponseBuilder status headers (ByteString.fromLazyByteString lastResponse)
                            event <- MVar.newEmptyMVar
                            pendingChanges <- newIORef mempty

                            let session = AutoRefreshSessionWithChanges { id, renderView, event, tables, lastResponse, lastPing, pendingChanges, shouldRefresh = shouldRefresh' }
                            modifyIORef' autoRefreshServer (\s -> s { sessions = session:s.sessions })
                            async (gcSessions autoRefreshServer)

                            registerRowNotificationTrigger ?touchedTables autoRefreshServer

                            throw (ResponseException response')
                        _   -> error "Unimplemented WAI response type."

                runAction `Exception.catch` handleResponse
        AutoRefreshEnabled {} -> do
            runAction

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        availableSessions <- getAvailableSessions (autoRefreshServerFromRequest request)

        Log.info
            ("AutoRefresh: ws connect sessionId=" <> tshow sessionId <> " available=" <> tshow (length availableSessions))

        when (sessionId `elem` availableSessions) do
            putContext (AutoRefreshEnabled sessionId)
            session <- getSessionById (autoRefreshServerFromRequest request) sessionId

            let handleResponseException lastResponse (ResponseException response) = case response of
                    Wai.ResponseBuilder status headers builder -> do
                        rawHtml <- pure (ByteString.toLazyByteString builder)
                        html <- ensureAutoRefreshMeta headers rawHtml

                        let changed = html /= lastResponse
                        Log.info
                            ("AutoRefresh: render status=" <> tshow status <> " len=" <> tshow (LBS.length html) <> " changed=" <> tshow changed)

                        when (html /= lastResponse) do
                            Log.info ("AutoRefresh: send sessionId=" <> tshow sessionId)
                            sendTextData html
                            updateSession (autoRefreshServerFromRequest request) sessionId (\session' -> session' { lastResponse = html })
                    _   -> error "Unimplemented WAI response type."

            let currentRequest = ?request
            let dummyRespond _ = error "AutoRefresh: respond should not be called directly"
            case session of
                AutoRefreshSession { renderView, event, lastResponse } -> do
                    async $ forever do
                        MVar.takeMVar event
                        Log.info ("AutoRefresh: render start sessionId=" <> tshow sessionId)
                        let renderAction =
                                (renderView currentRequest dummyRespond) `catch` handleResponseException lastResponse
                        let renderActionWithErrors =
                                renderAction `Exception.catch` \(err :: Exception.SomeException) -> do
                                    Log.error
                                        ("AutoRefresh: render exception sessionId=" <> tshow sessionId <> " err=" <> tshow err)
                        result <- Timeout.timeout (5 * 1000 * 1000) renderActionWithErrors
                        case result of
                            Nothing -> Log.error ("AutoRefresh: render timeout sessionId=" <> tshow sessionId)
                            Just _ -> Log.info ("AutoRefresh: render finished sessionId=" <> tshow sessionId)
                        pure ()
                AutoRefreshSessionWithChanges { renderView, event, lastResponse, pendingChanges, shouldRefresh } -> do
                    async $ forever do
                        MVar.takeMVar event
                        changes <- atomicModifyIORef' pendingChanges (\current -> (mempty, current))
                        shouldRender <- shouldRefresh changes
                        when shouldRender do
                            Log.info ("AutoRefresh: render start sessionId=" <> tshow sessionId)
                            let renderAction =
                                    (renderView currentRequest dummyRespond) `catch` handleResponseException lastResponse
                            let renderActionWithErrors =
                                    renderAction `Exception.catch` \(err :: Exception.SomeException) -> do
                                        Log.error
                                            ("AutoRefresh: render exception sessionId=" <> tshow sessionId <> " err=" <> tshow err)
                            result <- Timeout.timeout (5 * 1000 * 1000) renderActionWithErrors
                            case result of
                                Nothing -> Log.error ("AutoRefresh: render timeout sessionId=" <> tshow sessionId)
                                Just _ -> Log.info ("AutoRefresh: render finished sessionId=" <> tshow sessionId)
                        pure ()

            pure ()

        -- Keep the connection open until it's killed and the onClose is called
        forever receiveDataMessage

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        updateSession (autoRefreshServerFromRequest request) sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                let autoRefreshServer = autoRefreshServerFromRequest request
                modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\session -> session.id /= sessionId) server.sessions })
            AwaitingSessionID -> pure ()


registerNotificationTrigger :: (?modelContext :: ModelContext) => IORef (Set ByteString) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedTables <- (.subscribedTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)
    modifyIORef' autoRefreshServer (\server -> server { subscribedTables = server.subscribedTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <-  subscriptionRequired |> mapM (\table -> do
        let createTriggerSql = notificationTrigger table

        -- We need to add the trigger from the main IHP database role other we will get this error:
        -- ERROR:  permission denied for schema public
        withRowLevelSecurityDisabled do
            sqlExec createTriggerSql ()

        pgListener |> PGListener.subscribe (channelName table) \notification -> do
                sessions <- (.sessions) <$> readIORef autoRefreshServer
                sessions
                    |> mapMaybe (\session -> case session of
                        AutoRefreshSession { tables, event } | table `Set.member` tables -> Just event
                        AutoRefreshSession {} -> Nothing
                        AutoRefreshSessionWithChanges {} -> Nothing)
                    |> mapM (\event -> MVar.tryPutMVar event ())
                pure ())
    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()

registerRowNotificationTrigger :: (?modelContext :: ModelContext) => IORef (Set ByteString) -> IORef AutoRefreshServer -> IO ()
registerRowNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedRowTables <- (.subscribedRowTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> table `Set.notMember` subscribedRowTables)
    modifyIORef' autoRefreshServer (\server -> server { subscribedRowTables = server.subscribedRowTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- subscriptionRequired |> mapM (\table -> do
        primaryKeyColumns <- fetchPrimaryKeyColumns table
        when (null primaryKeyColumns) do
            error ("notificationRowTrigger: No primary keys found for " <> cs table)

        let createTriggerSql = notificationRowTrigger table primaryKeyColumns

        withRowLevelSecurityDisabled do
            sqlExec createTriggerSql ()

        pgListener |> PGListener.subscribeJSON (rowChannelName table) \payload -> do
                resolvedPayload <- resolveAutoRefreshPayload payload
                sessions <- (.sessions) <$> readIORef autoRefreshServer
                sessions |> mapM_ (handleRowChange table resolvedPayload)
                pure ())
    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()
    where
        handleRowChange table payload session = case session of
            AutoRefreshSessionWithChanges { tables, pendingChanges, event }
                | table `Set.member` tables -> do
                    modifyIORef' pendingChanges (insertRowChangeFromPayload table payload)
                    _ <- MVar.tryPutMVar event ()
                    pure ()
            AutoRefreshSessionWithChanges {} -> pure ()
            AutoRefreshSession {} -> pure ()

-- | Returns the ids of all sessions available to the client based on what sessions are found in the session cookie
getAvailableSessions :: (?request :: Request) => IORef AutoRefreshServer -> IO [UUID]
getAvailableSessions autoRefreshServer = do
    allSessions <- (.sessions) <$> readIORef autoRefreshServer
    text <- fromMaybe "" <$> getSession "autoRefreshSessions"
    let uuidCharCount = Text.length (UUID.toText UUID.nil)
    let allSessionIds = map (.id) allSessions
    text
        |> Text.chunksOf uuidCharCount
        |> mapMaybe UUID.fromText
        |> filter (\id -> id `elem` allSessionIds)
        |> pure

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
channelName :: ByteString -> ByteString
channelName tableName = "ar_did_change_" <> tableName

rowChannelName :: ByteString -> ByteString
rowChannelName tableName = "ar_did_change_row_" <> tableName

-- | Returns the sql code to set up a database trigger
notificationTrigger :: ByteString -> PG.Query
notificationTrigger tableName = PG.Query [i|
        BEGIN;
            CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
                BEGIN
                    PERFORM pg_notify('#{channelName tableName}', '');
                    RETURN new;
                END;
            $$ language plpgsql;
            DROP TRIGGER IF EXISTS #{insertTriggerName} ON #{tableName};
            CREATE TRIGGER #{insertTriggerName} AFTER INSERT ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{updateTriggerName} ON #{tableName};
            CREATE TRIGGER #{updateTriggerName} AFTER UPDATE ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{deleteTriggerName} ON #{tableName};
            CREATE TRIGGER #{deleteTriggerName} AFTER DELETE ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();

        COMMIT;
    |]
    where
        functionName = "ar_notify_did_change_" <> tableName
        insertTriggerName = "ar_did_insert_" <> tableName
        updateTriggerName = "ar_did_update_" <> tableName
        deleteTriggerName = "ar_did_delete_" <> tableName

notificationRowTrigger :: ByteString -> [ByteString] -> PG.Query
notificationRowTrigger tableName primaryKeyColumns = PG.Query [i|
        BEGIN;
            CREATE UNLOGGED TABLE IF NOT EXISTS large_pg_notifications (
                id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                payload TEXT DEFAULT NULL,
                created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
            );
            CREATE INDEX IF NOT EXISTS large_pg_notifications_created_at_index ON large_pg_notifications (created_at);
            CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
                DECLARE
                    row_id jsonb;
                    payload TEXT;
                    large_pg_notification_id UUID;
                BEGIN
                    IF (TG_OP = 'DELETE') THEN
                        row_id := #{oldRowIdExpression};
                        payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'old', to_jsonb(OLD))::text;
                        IF octet_length(payload) > 7800 THEN
                            INSERT INTO large_pg_notifications (payload) VALUES (payload) RETURNING id INTO large_pg_notification_id;
                            payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'payloadId', large_pg_notification_id::text)::text;
                            DELETE FROM large_pg_notifications WHERE created_at < CURRENT_TIMESTAMP - interval '30s';
                        END IF;
                        PERFORM pg_notify('#{rowChannelName tableName}', payload);
                        RETURN OLD;
                    ELSE
                        row_id := #{newRowIdExpression};
                        IF (TG_OP = 'UPDATE') THEN
                            payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'old', to_jsonb(OLD), 'new', to_jsonb(NEW))::text;
                        ELSE
                            payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'new', to_jsonb(NEW))::text;
                        END IF;
                        IF octet_length(payload) > 7800 THEN
                            INSERT INTO large_pg_notifications (payload) VALUES (payload) RETURNING id INTO large_pg_notification_id;
                            payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'payloadId', large_pg_notification_id::text)::text;
                            DELETE FROM large_pg_notifications WHERE created_at < CURRENT_TIMESTAMP - interval '30s';
                        END IF;
                        PERFORM pg_notify('#{rowChannelName tableName}', payload);
                        RETURN NEW;
                    END IF;
                END;
            $$ language plpgsql;
            DROP TRIGGER IF EXISTS #{insertTriggerName} ON #{tableName};
            CREATE TRIGGER #{insertTriggerName} AFTER INSERT ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{updateTriggerName} ON #{tableName};
            CREATE TRIGGER #{updateTriggerName} AFTER UPDATE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{deleteTriggerName} ON #{tableName};
            CREATE TRIGGER #{deleteTriggerName} AFTER DELETE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();

        COMMIT;
    |]
    where
        functionName = "ar_notify_row_change_" <> tableName
        insertTriggerName = "ar_did_insert_row_" <> tableName
        updateTriggerName = "ar_did_update_row_" <> tableName
        deleteTriggerName = "ar_did_delete_row_" <> tableName
        newRowIdExpression = rowIdExpression "NEW" primaryKeyColumns
        oldRowIdExpression = rowIdExpression "OLD" primaryKeyColumns

rowIdExpression :: ByteString -> [ByteString] -> ByteString
rowIdExpression recordAlias primaryKeyColumns =
    case primaryKeyColumns of
        [] -> error "notificationRowTrigger: No primary keys found"
        [column] -> "to_jsonb(" <> qualifiedColumn column <> ")"
        columns -> "jsonb_build_array(" <> B8.intercalate ", " (map qualifiedColumn columns) <> ")"
    where
        qualifiedColumn column = recordAlias <> ".\"" <> column <> "\""

fetchPrimaryKeyColumns :: (?modelContext :: ModelContext) => ByteString -> IO [ByteString]
fetchPrimaryKeyColumns tableName = do
    let query = "SELECT a.attname FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = ?::regclass AND i.indisprimary ORDER BY array_position(i.indkey, a.attnum)"
    rows <- (sqlQuery (PG.Query query) (PG.Only (cs tableName :: Text)) :: IO [PG.Only Text])
    pure (map (cs . (\(PG.Only name) -> name)) rows)

resolveAutoRefreshPayload :: (?modelContext :: ModelContext) => AutoRefreshRowChangePayload -> IO AutoRefreshRowChangePayload
resolveAutoRefreshPayload payload = case payload.payloadLargePayloadId of
    Nothing        -> pure payload
    Just payloadId -> fetchAutoRefreshPayload payloadId

fetchAutoRefreshPayload :: (?modelContext :: ModelContext) => UUID.UUID -> IO AutoRefreshRowChangePayload
fetchAutoRefreshPayload payloadId = do
    (payload :: ByteString) <- sqlQueryScalar "SELECT payload FROM large_pg_notifications WHERE id = ? LIMIT 1" (PG.Only payloadId)
    case Aeson.eitherDecodeStrict' payload of
        Left errorMessage -> error ("AutoRefresh: Unable to decode payload: " <> cs errorMessage)
        Right result -> pure result

ensureAutoRefreshMeta :: (?context :: ControllerContext, ?request :: Request) => ResponseHeaders -> LByteString -> IO LByteString
ensureAutoRefreshMeta headers html
    | not (isHtmlResponse headers) = pure html
    | BS.isInfixOf "ihp-auto-refresh-id" (LBS.toStrict html) = pure html
    | otherwise = do
        meta <- renderAutoRefreshMeta
        if LBS.null meta
            then pure html
            else pure (insertAutoRefreshMetaForRequest meta html)

renderAutoRefreshMeta :: (?context :: ControllerContext) => IO LByteString
renderAutoRefreshMeta = do
    frozenContext <- freeze ?context
    let ?context = frozenContext
    let metaText = LazyText.toStrict (BlazeText.renderHtml autoRefreshMeta)
    pure (encodeUtf8Text metaText)

insertAutoRefreshMeta :: LByteString -> LByteString -> LByteString
insertAutoRefreshMeta meta html =
    let metaText = decodeUtf8Text meta
        htmlText = decodeUtf8Text html
        resultText = insertMetaText metaText htmlText
    in encodeUtf8Text resultText

insertAutoRefreshMetaForRequest :: (?context :: ControllerContext, ?request :: Request) => LByteString -> LByteString -> LByteString
insertAutoRefreshMetaForRequest meta html =
    if isHtmxRequest
        then insertAutoRefreshMetaFragment meta html
        else insertAutoRefreshMeta meta html

insertAutoRefreshMetaFragment :: LByteString -> LByteString -> LByteString
insertAutoRefreshMetaFragment meta html =
    let metaText = decodeUtf8Text meta
        htmlText = decodeUtf8Text html
    in encodeUtf8Text (metaText <> htmlText)

insertMetaText :: Text -> Text -> Text
insertMetaText meta html =
    case findHeadInsertionIndex html of
        Just index -> Text.take index html <> meta <> Text.drop index html
        Nothing    -> meta <> html

findHeadInsertionIndex :: Text -> Maybe Int
findHeadInsertionIndex html =
    let lowerHtml = Text.toLower html
        (beforeHead, rest) = Text.breakOn "<head" lowerHtml
    in if Text.null rest
        then Nothing
        else
            let afterHead = Text.dropWhile (/= '>') rest
            in if Text.null afterHead
                then Nothing
                else
                    let headOpenLen = Text.length rest - Text.length afterHead
                    in Just (Text.length beforeHead + headOpenLen + 1)

isHtmlResponse :: ResponseHeaders -> Bool
isHtmlResponse headers = case lookup hContentType headers of
    Just value -> "text/html" `BS.isPrefixOf` value
    Nothing    -> False

isHtmxRequest :: (?context :: ControllerContext, ?request :: Request) => Bool
isHtmxRequest = isJust (getHeader "HX-Request")

decodeUtf8Text :: LByteString -> Text
decodeUtf8Text = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode . LBS.toStrict

encodeUtf8Text :: Text -> LByteString
encodeUtf8Text = LBS.fromStrict . TextEncoding.encodeUtf8

autoRefreshVaultKey :: Vault.Key (IORef AutoRefreshServer)
autoRefreshVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshVaultKey #-}

initAutoRefreshMiddleware :: PGListener.PGListener -> IO Middleware
initAutoRefreshMiddleware pgListener = do
    autoRefreshServer <- newIORef (newAutoRefreshServer pgListener)
    pure \app request respond -> do
        let request' = request { vault = Vault.insert autoRefreshVaultKey autoRefreshServer request.vault }
        app request' respond

autoRefreshServerFromRequest :: Request -> IORef AutoRefreshServer
autoRefreshServerFromRequest request =
    case Vault.lookup autoRefreshVaultKey request.vault of
        Just server -> server
        Nothing -> error "AutoRefresh middleware not initialized. Please make sure you have added the AutoRefresh middleware to your application."
