{-|
Module: Test.AutoRefreshSpec
Tests that AutoRefresh preserves query parameters when re-rendering
with a bare WebSocket request (no query params).
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.AutoRefreshSpec where
import qualified Data.Aeson as Aeson
import Test.Hspec
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.ControllerPrelude hiding (get, request)
import Network.Wai
import Network.HTTP.Types
import IHP.AutoRefresh (getSessionById, globalAutoRefreshServerVar, notificationRowTriggerStatements, sessionResponseHasChanged, updateSession)
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
    -> IO SResponse
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
    runSession (request baseRequest) (middlewareStack controllerApp)

testLogger :: FastLogger
testLogger = noopLogger

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "AutoRefresh" do
        describe "autoRefreshMeta" do
            it "renders the ihp-auto-refresh-id meta tag on the initial response" $ withContext do
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                PGListener.withPGListener "" testLogger \pgListener -> do
                    response <- callActionWithQueryParams pgListener ShowItemHtmlAction [("marketId", "abc-123")]
                    let bodyBs = LBS.toStrict (simpleBody response)
                    BS.isInfixOf "ihp-auto-refresh-id" bodyBs `shouldBe` True

                    MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

        describe "renderView" do
            it "should preserve query parameters when re-rendering with a websocket request" $ withContext do
                -- Clean up any leftover global state from previous tests
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                PGListener.withPGListener "" testLogger \pgListener -> do
                    -- 1. Call the action with query params — this triggers autoRefresh
                    --    which stores a session with renderView
                    response <- callActionWithQueryParams pgListener ShowItemAction [("marketId", "abc-123")]
                    cs (simpleBody response) `shouldBe` ("abc-123" :: Text)

                    -- 2. Extract the stored renderView from the AutoRefreshSession
                    maybeServerRef <- MVar.readMVar globalAutoRefreshServerVar
                    serverRef <- case maybeServerRef of
                        Just ref -> pure ref
                        Nothing -> error "AutoRefreshServer was not created"

                    server <- readIORef serverRef
                    session <- case server.sessions of
                        (s:_) -> pure s
                        [] -> error "No AutoRefresh sessions found"

                    -- 3. Call renderView with a bare request (simulating WebSocket re-render)
                    --    The WebSocket request has NO query params — this is the bug scenario
                    reResponse <- runSession (request defaultRequest) session.renderView
                    -- If query params are NOT preserved, this would throw ParamNotFoundException
                    cs (simpleBody reResponse) `shouldBe` ("abc-123" :: Text)

                    -- Cleanup
                    MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

        describe "graceful degradation without PGListener" do
            it "should run the action without crashing when PGListener is not available" $ withContext do
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                response <- callActionWithParams ShowItemAction [("marketId", "degraded-ok")]
                body <- responseBody response
                cs body `shouldBe` ("degraded-ok" :: Text)

                -- Verify autoRefresh skipped subscription machinery entirely
                maybeServerRef <- MVar.readMVar globalAutoRefreshServerVar
                case maybeServerRef of
                    Nothing -> pure ()
                    Just _ -> expectationFailure "Expected globalAutoRefreshServerVar to be Nothing"

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
                            , subscribedRowTables = mempty
                            , pgListener = error "pgListener unused in session state test"
                            }

                updateSession serverRef UUID.nil (\currentSession -> currentSession { lastResponse = "unresolved" })

                sessionResponseHasChanged serverRef UUID.nil "resolved" `shouldReturn` True
                sessionResponseHasChanged serverRef UUID.nil "unresolved" `shouldReturn` False

            it "finds fine-grained sessions by id" $ withContext do
                event <- MVar.newEmptyMVar
                pendingChanges <- newIORef (Just mempty)
                now <- getCurrentTime
                let session =
                        AutoRefreshSessionWithChanges
                            { id = UUID.nil
                            , renderView = \_ respond -> respond (Wai.responseLBS status200 [] "")
                            , event
                            , tables = mempty
                            , lastResponse = ""
                            , lastPing = now
                            , pendingChanges
                            , shouldRefresh = \_ -> pure True
                            }
                serverRef <-
                    newIORef
                        AutoRefreshServer
                            { subscriptions = []
                            , sessions = [session]
                            , subscribedTables = mempty
                            , subscribedRowTables = mempty
                            , pgListener = error "pgListener unused in fine-grained session test"
                            }

                found <- getSessionById serverRef UUID.nil
                found.id `shouldBe` UUID.nil

        describe "fine-grained change sets" do
            it "keeps runtime payload storage outside the application public schema" $ withContext do
                let statements = notificationRowTriggerStatements "tasks"

                statements `shouldSatisfy` any ("ihp_runtime.large_pg_notifications" `isInfixOf`)
                statements `shouldSatisfy` all (not . ("public.large_pg_notifications" `isInfixOf`))

            it "exposes new and old row fields using model field names" $ withContext do
                let oldRow = Aeson.object ["project_id" Aeson..= ("before" :: Text)]
                    newRow = Aeson.object ["project_id" Aeson..= ("after" :: Text)]
                    payload =
                        AutoRefreshRowChangePayload
                            { payloadOperation = AutoRefreshUpdate
                            , payloadOldRow = Just oldRow
                            , payloadNewRow = Just newRow
                            , payloadLargePayloadId = Nothing
                            }
                    changeSet = insertRowChangeFromPayload "tasks" payload mempty
                    [change] = changesForTable "tasks" changeSet

                rowFieldNew @"projectId" change `shouldBe` Just ("after" :: Text)
                rowFieldOld @"projectId" change `shouldBe` Just ("before" :: Text)

            it "keeps changes scoped to their table" $ withContext do
                let payload =
                        AutoRefreshRowChangePayload
                            { payloadOperation = AutoRefreshInsert
                            , payloadOldRow = Nothing
                            , payloadNewRow = Just (Aeson.object ["id" Aeson..= (1 :: Int)])
                            , payloadLargePayloadId = Nothing
                            }
                    changeSet = insertRowChangeFromPayload "tasks" payload mempty

                anyChangeOnTable "tasks" changeSet `shouldBe` True
                anyChangeOnTable "projects" changeSet `shouldBe` False

            it "checks fields across all tables with predicates" $ withContext do
                let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a004"
                    userPayload =
                        AutoRefreshRowChangePayload
                            { payloadOperation = AutoRefreshInsert
                            , payloadOldRow = Nothing
                            , payloadNewRow = Just (Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId])
                            , payloadLargePayloadId = Nothing
                            }
                    projectPayload =
                        AutoRefreshRowChangePayload
                            { payloadOperation = AutoRefreshUpdate
                            , payloadOldRow = Nothing
                            , payloadNewRow = Just (Aeson.object ["id" Aeson..= ("p-1" :: Text), "user_id" Aeson..= userId])
                            , payloadLargePayloadId = Nothing
                            }
                    changeSet =
                        mempty
                            |> insertRowChangeFromPayload "projects" projectPayload
                            |> insertRowChangeFromPayload "users" userPayload

                anyChangeWithField @"userId" (== userId) changeSet `shouldBe` True

            it "supports custom field predicates" $ withContext do
                let payload =
                        AutoRefreshRowChangePayload
                            { payloadOperation = AutoRefreshUpdate
                            , payloadOldRow = Nothing
                            , payloadNewRow = Just (Aeson.object ["id" Aeson..= ("u-1" :: Text), "status" Aeson..= ("archived" :: Text)])
                            , payloadLargePayloadId = Nothing
                            }
                    changeSet = insertRowChangeFromPayload "users" payload mempty

                anyChangeWithField @"status" (`elem` ["active" :: Text, "archived"]) changeSet `shouldBe` True
                anyChangeWithField @"status" (== ("active" :: Text)) changeSet `shouldBe` False
