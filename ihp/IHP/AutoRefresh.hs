{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.ControllerSupport
import IHP.ApplicationContext
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Char8 as BS
import IHP.ModelSupport
import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import IHP.WebSocket
import IHP.Controller.Context
import IHP.Controller.Response 
import qualified IHP.PGListener as PGListener
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PGN
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import Data.String.Interpolate.IsString

initAutoRefresh :: (?context :: ControllerContext, ?applicationContext :: ApplicationContext) => IO ()
initAutoRefresh = do
    putContext AutoRefreshDisabled
    putContext ?applicationContext.autoRefreshServer

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefresh runAction = do
    autoRefreshState <- fromContext @AutoRefreshState
    autoRefreshServer <- fromContext @(IORef AutoRefreshServer)

    case autoRefreshState of
        AutoRefreshDisabled -> do
            availableSessions <- getAvailableSessions autoRefreshServer

            id <- UUID.nextRandom

            -- We save the current state of the controller context here. This includes e.g. all current
            -- flash messages, the current user, ...
            --
            -- This frozen context is used as a "template" inside renderView to make a new controller context
            -- with the exact same content we had when rendering the initial page, whenever we do a server-side re-rendering
            frozenControllerContext <- freeze ?context

            let renderView = \requestContext -> do
                    controllerContext <- unfreeze frozenControllerContext
                    let ?context = controllerContext { requestContext }
                    action ?theAction

            putContext (AutoRefreshEnabled id)

            -- We save the allowed session ids to the session cookie to only grant a client access
            -- to sessions it initially opened itself
            --
            -- Otherwise you might try to guess session UUIDs to access other peoples auto refresh sessions
            setSession "autoRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")

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
                            lastResponse <- Exception.evaluate (ByteString.toLazyByteString builder)

                            event <- MVar.newEmptyMVar
                            recordIdsByTable <- readIORef ?trackedRecordIdsByTable
                            selectQueriesByTable <- readIORef ?trackedSelectsByTable

                            let session = AutoRefreshSession { id, renderView, event, tables, recordIdsByTable, selectQueriesByTable, lastResponse, lastPing }
                            modifyIORef' autoRefreshServer (\s -> s { sessions = session:s.sessions } )
                            async (gcSessions autoRefreshServer)

                            registerNotificationTrigger ?touchedTables autoRefreshServer

                            throw exception
                        _   -> error "Unimplemented WAI response type."

                runAction `Exception.catch` handleResponse
        AutoRefreshEnabled {} -> do
            -- When this function calls the 'action ?theAction' in the other case
            -- we will evaluate this branch
            runAction

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        availableSessions <- getAvailableSessions ?applicationContext.autoRefreshServer

        when (sessionId `elem` availableSessions) do
            AutoRefreshSession { renderView, event, lastResponse } <- getSessionById sessionId

            let handleResponseException (ResponseException response) = case response of
                    Wai.ResponseBuilder status headers builder -> do
                        let html = ByteString.toLazyByteString builder

                        when (html /= lastResponse) do
                            sendTextData html
                            updateSession sessionId (\session -> session { lastResponse = html })
                    _   -> error "Unimplemented WAI response type."

            async $ forever do
                MVar.takeMVar event
                let requestContext = ?context.requestContext
                (renderView requestContext) `catch` handleResponseException
                pure ()

            pure ()

        -- Keep the connection open until it's killed and the onClose is called
        forever receiveDataMessage

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        updateSession sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                let autoRefreshServer = ?applicationContext.autoRefreshServer
                modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\AutoRefreshSession { id } -> id /= sessionId) server.sessions })
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
                let idText = cs (PGN.notificationData notification) :: Text
                sessions <- (.sessions) <$> readIORef autoRefreshServer
                forEach sessions \session -> do
                    when (table `Set.member` session.tables) do
                        let watchedIds = HashMap.lookupDefault Set.empty table session.recordIdsByTable
                        if Set.member idText watchedIds then do
                            _ <- MVar.tryPutMVar session.event ()
                            pure ()
                        else do
                            let selects = HashMap.lookupDefault [] table session.selectQueriesByTable
                            existsBools <- forM selects (\(sql, params) -> do
                                let membershipSQL = PG.Query ("SELECT EXISTS(SELECT 1 FROM (" <> cs sql <> ") AS records WHERE (records.id)::text = ?) AS _exists")
                                [PG.Only exists] <- sqlQuery membershipSQL (params <> [PGTF.toField idText])
                                pure (exists :: Bool))
                            let shouldRerender = or existsBools
                            when shouldRerender do
                                -- update watched ids in server state
                                let updateSession s = if s.id == session.id then s { recordIdsByTable = HashMap.insert table (Set.insert idText watchedIds) s.recordIdsByTable } else s
                                modifyIORef' autoRefreshServer (\server -> server { sessions = map updateSession server.sessions })
                                _ <- MVar.tryPutMVar session.event ()
                                pure ()
                pure ())
    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()

-- | Returns the ids of all sessions available to the client based on what sessions are found in the session cookie
getAvailableSessions :: (?context :: ControllerContext) => IORef AutoRefreshServer -> IO [UUID]
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
getSessionById :: (?applicationContext :: ApplicationContext) => UUID -> IO AutoRefreshSession
getSessionById sessionId = do
    autoRefreshServer <- readIORef ?applicationContext.autoRefreshServer
    autoRefreshServer.sessions
        |> find (\AutoRefreshSession { id } -> id == sessionId)
        |> Maybe.fromMaybe (error "getSessionById: Could not find the session")
        |> pure

-- | Applies a update function to a session specified by its session id
updateSession :: (?applicationContext :: ApplicationContext) => UUID -> (AutoRefreshSession -> AutoRefreshSession) -> IO ()
updateSession sessionId updateFunction = do
    let server = ?applicationContext.autoRefreshServer
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
isSessionExpired now AutoRefreshSession { lastPing } = (now `diffUTCTime` lastPing) > (secondsToNominalDiffTime 60)

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: ByteString -> ByteString
channelName tableName = "ar_did_change_" <> tableName

-- | Returns the sql code to set up a database trigger
notificationTrigger :: ByteString -> PG.Query
notificationTrigger tableName = PG.Query [i|
        BEGIN;
            CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
                BEGIN
                    IF TG_OP = 'INSERT' THEN
                    PERFORM pg_notify('#{channelName tableName}', NEW.id::text);
                ELSIF TG_OP = 'UPDATE' THEN
                    PERFORM pg_notify('#{channelName tableName}', NEW.id::text);
                ELSIF TG_OP = 'DELETE' THEN
                    PERFORM pg_notify('#{channelName tableName}', OLD.id::text);
                END IF;
                    RETURN new;
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
        functionName = "ar_notify_did_change_" <> tableName
        insertTriggerName = "ar_did_insert_" <> tableName
        updateTriggerName = "ar_did_update_" <> tableName
        deleteTriggerName = "ar_did_delete_" <> tableName