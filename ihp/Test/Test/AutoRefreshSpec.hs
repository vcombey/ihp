{-|
Module: Test.AutoRefreshSpec
Tests that AutoRefresh preserves query parameters when re-rendering
with a bare WebSocket request (no query params).
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.AutoRefreshSpec where
import Test.Hspec
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.ControllerPrelude hiding (get, request)
import Network.Wai
import Network.HTTP.Types
import IHP.AutoRefresh (getAvailableSessions, getOrCreateAutoRefreshServer, sessionResponseHasChanged, updateSession, processAutoRefreshWithPayload, commitAutoRefreshTables, getSessionById)
import IHP.AutoRefresh.ChangeSet (AutoRefreshChangeSet)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import Control.Monad (void)
import IHP.AutoRefresh.Types
import IHP.AutoRefresh.View (autoRefreshMeta)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified IHP.PGListener as PGListener
import System.Log.FastLogger (FastLogger)
import IHP.Server (initMiddlewareStack)
import Network.Wai.Test (runSession, request, SResponse(..), simpleBody)
import IHP.Test.Mocking
import qualified Data.UUID as UUID
import qualified Data.Set as Set
import qualified Network.Wai as Wai

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = ShowItemAction
    | ShowItemHtmlAction
  deriving (Eq, Show, Data)

instance Controller TestController where
    action ShowItemAction = autoRefresh do
        let marketId = param @Text "marketId"
        renderPlain (cs marketId)
    action ShowItemHtmlAction = autoRefresh do
        let marketId = param @Text "marketId"
        let meta = autoRefreshMeta
        respondHtml [hsx|<html><head>{meta}</head><body>{marketId}</body></html>|]

instance AutoRoute TestController

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController ]

instance InitControllerContext WebApplication where
  initContext = pure ()

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

config :: ConfigBuilder
config = do
    option Development
    option (AppPort 8000)

-- | Helper that calls a controller action with query parameters (GET-style)
-- and passes a PGListener to the middleware stack so autoRefresh can work.
callActionWithQueryParams
    :: forall application controller
     . ( Controller controller
       , ContextParameters application
       , Typeable application
       , Typeable controller
       )
    => PGListener.PGListener
    -> controller
    -> [(ByteString, ByteString)]
    -> IO (SResponse, IORef AutoRefreshServer)
callActionWithQueryParams pgListener controller queryParams = do
    let MockContext { frameworkConfig, modelContext } = ?mocking

    let baseRequest = ?request
            { Wai.queryString = map (\(k,v) -> (k, Just v)) queryParams
            , Wai.rawQueryString = renderSimpleQuery True queryParams
            }

    let controllerApp req respond = do
            let ?request = req
            let ?respond = respond
            runActionWithNewContext controller

    middlewareStack <- initMiddlewareStack frameworkConfig modelContext (Just pgListener)
    server <- captureServer middlewareStack
    response <- runSession (request baseRequest) (middlewareStack controllerApp)
    pure (response, server)

testLogger :: FastLogger
testLogger = noopLogger

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "AutoRefresh" do
        describe "application lifecycle" do
            it "creates fresh state after the previous application's listener has stopped" $ withContext do
                let MockContext { frameworkConfig, modelContext } = ?mocking
                oldServer <- PGListener.withPGListener "" testLogger \pgListener -> do
                    middleware <- initMiddlewareStack frameworkConfig modelContext (Just pgListener)
                    server <- captureServer middleware
                    ((== server) <$> captureServer middleware) `shouldReturn` True
                    session <- payloadSession (const (pure True))
                    modifyIORef' server (\state -> state { sessions = [session], subscribedTables = Set.singleton "projects" })
                    pure server
                PGListener.withPGListener "" testLogger \pgListener -> do
                    middleware <- initMiddlewareStack frameworkConfig modelContext (Just pgListener)
                    server <- captureServer middleware
                    (server == oldServer) `shouldBe` False
                    current <- readIORef server
                    Async.asyncThreadId current.pgListener.notifyLoopAsync
                        `shouldBe` Async.asyncThreadId pgListener.notifyLoopAsync
                    length current.sessions `shouldBe` 0
                    current.subscribedTables `shouldBe` mempty

            it "keeps overlapping application instances independent" $ withContext do
                let MockContext { frameworkConfig, modelContext } = ?mocking
                PGListener.withPGListener "" testLogger \firstListener ->
                    PGListener.withPGListener "" testLogger \secondListener -> do
                        first <- initMiddlewareStack frameworkConfig modelContext (Just firstListener)
                        second <- initMiddlewareStack frameworkConfig modelContext (Just secondListener)
                        firstServer <- captureServer first
                        secondServer <- captureServer second
                        (firstServer == secondServer) `shouldBe` False
                        ((== firstServer) <$> captureServer first) `shouldReturn` True
                        ((== secondServer) <$> captureServer second) `shouldReturn` True

        describe "getAvailableSessions" do
            let activeId = UUID.fromWords 0 0 0 1
            let staleId = UUID.fromWords 0 0 0 2
            let unknownId = UUID.fromWords 0 0 0 3
            let activeText = cs (UUID.toText activeId)
            forM_
                [ ("uses the header snapshot instead of stale cookie entries", [("X-IHP-Auto-Refresh-Sessions", activeText <> cs (UUID.toText unknownId))], [], [activeId])
                , ("uses the websocket query snapshot instead of stale cookie entries", [], [("autoRefreshSessions", Just activeText)], [activeId])
                , ("deduplicates explicit snapshots", [("X-IHP-Auto-Refresh-Sessions", activeText)], [("autoRefreshSessions", Just activeText)], [activeId])
                , ("keeps the cookie fallback for clients without an explicit snapshot", [], [], [activeId, staleId])
                ] \(description, headers, queryParams, expected) ->
                    it description $ withContext do
                        event <- MVar.newEmptyMVar
                        now <- getCurrentTime
                        let session id = AutoRefreshSession
                                { id
                                , renderView = \_ respond -> respond (Wai.responseLBS status200 [] "")
                                , event
                                , tables = mempty
                                , lastResponse = ""
                                , lastPing = now
                                }
                        serverRef <- newIORef AutoRefreshServer
                            { subscriptions = []
                            , sessions = map session [activeId, staleId]
                            , subscribedTables = mempty
                            , pgListener = error "pgListener unused in session selection test"
                            }
                        setSession "autoRefreshSessions" (UUID.toText activeId <> UUID.toText staleId)
                        let clientRequest = ?request { requestHeaders = headers, queryString = queryParams }
                        let ?request = clientRequest
                        getAvailableSessions serverRef `shouldReturn` expected

        describe "autoRefreshMeta" do
            it "renders the ihp-auto-refresh-id meta tag on the initial response" $ withContext do
                PGListener.withPGListener "" testLogger \pgListener -> do
                    (response, _) <- callActionWithQueryParams pgListener ShowItemHtmlAction [("marketId", "abc-123")]
                    let bodyBs = LBS.toStrict (simpleBody response)
                    BS.isInfixOf "ihp-auto-refresh-id" bodyBs `shouldBe` True

        describe "renderView" do
            it "should preserve query parameters when re-rendering with a websocket request" $ withContext do
                PGListener.withPGListener "" testLogger \pgListener -> do
                    -- 1. Call the action with query params — this triggers autoRefresh
                    --    which stores a session with renderView
                    (response, serverRef) <- callActionWithQueryParams pgListener ShowItemAction [("marketId", "abc-123")]
                    cs (simpleBody response) `shouldBe` ("abc-123" :: Text)

                    -- 2. Extract the stored renderView from the AutoRefreshSession
                    server <- readIORef serverRef
                    session <- case server.sessions of
                        (s:_) -> pure s
                        [] -> error "No AutoRefresh sessions found"

                    -- 3. Call renderView with a bare request (simulating WebSocket re-render)
                    --    The WebSocket request has NO query params — this is the bug scenario
                    reResponse <- runSession (request defaultRequest) session.renderView
                    -- If query params are NOT preserved, this would throw ParamNotFoundException
                    cs (simpleBody reResponse) `shouldBe` ("abc-123" :: Text)

        describe "graceful degradation without PGListener" do
            it "should run the action without crashing when PGListener is not available" $ withContext do
                response <- callActionWithParams ShowItemAction [("marketId", "degraded-ok")]
                body <- responseBody response
                cs body `shouldBe` ("degraded-ok" :: Text)

        describe "session state tracking" do
            it "should compare re-rendered html against the latest session response" $ withContext do
                event <- MVar.newEmptyMVar
                now <- getCurrentTime
                let session =
                        AutoRefreshSession
                            { id = UUID.nil
                            , renderView = \_ respond -> respond (Wai.responseLBS status200 [] "")
                            , event
                            , tables = mempty
                            , lastResponse = "resolved"
                            , lastPing = now
                            }
                serverRef <-
                    newIORef
                        AutoRefreshServer
                            { subscriptions = []
                            , sessions = [session]
                            , subscribedTables = mempty
                            , pgListener = error "pgListener unused in session state test"
                            }

                updateSession serverRef UUID.nil (\currentSession -> currentSession { lastResponse = "unresolved" })

                sessionResponseHasChanged serverRef UUID.nil "resolved" `shouldReturn` True
                sessionResponseHasChanged serverRef UUID.nil "unresolved" `shouldReturn` False

        describe "payload filtering without SQL" do
            forM_ [(False, Nothing), (True, Just ())] \(decision, expected) ->
                it ("obeys the predicate: " <> cs (show decision)) $ withContext do
                    session <- payloadSession (const (pure decision))
                    server <- payloadServer session
                    processAutoRefreshWithPayload server "projects" "{\"op\":\"insert\",\"new\":{\"id\":1}}"
                    MVar.tryTakeMVar session.event `shouldReturn` expected

            it "invalidates unknown payloads without calling the predicate" $ withContext do
                session <- payloadSession (\_ -> error "Unknown payload must not reach predicate")
                server <- payloadServer session
                processAutoRefreshWithPayload server "projects" "legacy"
                MVar.tryTakeMVar session.event `shouldReturn` Just ()

            it "refreshes if evaluating a predicate result throws" $ withContext do
                session <- payloadSession (\_ -> pure (error "invalid predicate result"))
                server <- payloadServer session
                processAutoRefreshWithPayload server "projects" "{\"op\":\"insert\",\"new\":{\"id\":1}}"
                MVar.tryTakeMVar session.event `shouldReturn` Just ()

            it "propagates asynchronous cancellation from a predicate" $ withContext do
                session <- payloadSession (\_ -> Exception.throwIO Async.AsyncCancelled)
                server <- payloadServer session
                processAutoRefreshWithPayload server "projects" "{\"op\":\"insert\",\"new\":{\"id\":1}}"
                    `shouldThrow` (const True :: Async.AsyncCancelled -> Bool)
                MVar.tryTakeMVar session.event `shouldReturn` Nothing

            it "retains dependencies if registration fails" $ withContext do
                session <- payloadSession (const (pure False))
                server <- payloadServer session
                commitAutoRefreshTables (ioError (userError "registration failed")) server session.id mempty
                    `shouldThrow` anyIOException
                current <- getSessionById server session.id
                current.tables `shouldBe` session.tables

payloadSession :: (AutoRefreshChangeSet -> IO Bool) -> IO AutoRefreshSession
payloadSession predicate = do
    event <- MVar.newEmptyMVar
    lastPing <- getCurrentTime
    pure FilteredAutoRefreshSession
        { id = UUID.nil, renderView = \_ _ -> error "No render expected"
        , event, tables = Set.singleton "projects", lastResponse = "", lastPing
        , shouldRefresh = predicate
        }

payloadServer :: AutoRefreshSession -> IO (IORef AutoRefreshServer)
payloadServer session = newIORef ((newAutoRefreshServer (error "No database/listener available")) { sessions = [session] })

captureServer :: Middleware -> IO (IORef AutoRefreshServer)
captureServer middleware = do
    captured <- MVar.newEmptyMVar
    let app request respond = do
            let ?request = request
            getOrCreateAutoRefreshServer >>= MVar.putMVar captured
            respond (Wai.responseLBS status200 [] "")
    void (runSession (request defaultRequest) (middleware app))
    MVar.takeMVar captured
