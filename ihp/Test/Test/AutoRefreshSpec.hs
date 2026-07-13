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
import IHP.AutoRefresh (globalAutoRefreshServerVar, matchesInsertPayload, notificationTriggerStatements, sessionResponseHasChanged, shouldRefreshForPayload, updateSession)
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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Dynamic (toDyn)
import Data.Functor.Contravariant (contramap)
import qualified Data.Set as Set
import qualified Hasql.Encoders as Encoders

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
        describe "notification trigger setup" do
            it "keeps runtime objects outside the application public schema" $ withContext do
                let statements = notificationTriggerStatements "tasks"

                statements `shouldSatisfy` any ("ihp_runtime.large_pg_notifications" `isInfixOf`)
                statements `shouldSatisfy` any ("ihp_runtime.ar_notify_row_change_tasks" `isInfixOf`)
                statements `shouldSatisfy` all (not . ("public.large_pg_notifications" `isInfixOf`))
                statements `shouldSatisfy` any ("IF NOT EXISTS (SELECT 1 FROM pg_trigger" `isInfixOf`)
                statements `shouldSatisfy` all (not . ("DROP TRIGGER" `isInfixOf`))
                statements `shouldSatisfy` all (`notElem` ["BEGIN", "COMMIT"])

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
                            , trackedIds = mempty
                            , trackedConditions = mempty
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

        describe "matchesInsertPayload" do
            let mkRow pairs = AesonKeyMap.fromList [(AesonKey.fromText key, value) | (key, value) <- pairs]
                textParam value = Param (contramap (const value) (Encoders.param (Encoders.nonNullable Encoders.text)))
                uuidParam value = Param (contramap (const value) (Encoders.param (Encoders.nonNullable Encoders.uuid)))

            it "matches equality conditions" $ \_ -> do
                let row = mkRow [("project_id", Aeson.String "abc-123")]
                    condition = ColumnCondition "tasks.project_id" EqOp (textParam "abc-123") Nothing Nothing
                matchesInsertPayload condition row `shouldBe` True

            it "rejects non-matching equality conditions" $ \_ -> do
                let row = mkRow [("project_id", Aeson.String "other-id")]
                    condition = ColumnCondition "tasks.project_id" EqOp (textParam "abc-123") Nothing Nothing
                matchesInsertPayload condition row `shouldBe` False

            it "handles UUID values" $ \_ -> do
                let uuid = "a7a37bca-417b-21d5-38fc-7f9000efe79c" :: UUID.UUID
                    row = mkRow [("project_id", Aeson.String "a7a37bca-417b-21d5-38fc-7f9000efe79c")]
                    condition = ColumnCondition "tasks.project_id" EqOp (uuidParam uuid) Nothing Nothing
                matchesInsertPayload condition row `shouldBe` True

            it "evaluates compound conditions" $ \_ -> do
                let row = mkRow [("project_id", Aeson.String "abc"), ("status", Aeson.String "active")]
                    projectCondition = ColumnCondition "tasks.project_id" EqOp (textParam "abc") Nothing Nothing
                    activeCondition = ColumnCondition "tasks.status" EqOp (textParam "active") Nothing Nothing
                    inactiveCondition = ColumnCondition "tasks.status" EqOp (textParam "inactive") Nothing Nothing
                matchesInsertPayload (AndCondition projectCondition activeCondition) row `shouldBe` True
                matchesInsertPayload (AndCondition projectCondition inactiveCondition) row `shouldBe` False
                matchesInsertPayload (OrCondition activeCondition inactiveCondition) row `shouldBe` True

            it "falls back safely for unsupported operators and transforms" $ \_ -> do
                let row = mkRow [("name", Aeson.String "Hello")]
                    likeCondition = ColumnCondition "tasks.name" (LikeOp CaseSensitive) (textParam "%hello%") Nothing Nothing
                    lowerCondition = ColumnCondition "tasks.name" EqOp (textParam "hello") (Just "LOWER") Nothing
                matchesInsertPayload likeCondition row `shouldBe` True
                matchesInsertPayload lowerCondition row `shouldBe` True

            it "handles IS NULL" $ \_ -> do
                let nullCondition = ColumnCondition "tasks.deleted_at" IsOp (Literal "NULL") Nothing Nothing
                matchesInsertPayload nullCondition (mkRow [("deleted_at", Aeson.Null)]) `shouldBe` True
                matchesInsertPayload nullCondition (mkRow [("deleted_at", Aeson.String "2024-01-01")]) `shouldBe` False

        describe "shouldRefreshForPayload" do
            let mkRow pairs = AesonKeyMap.fromList [(AesonKey.fromText key, value) | (key, value) <- pairs]
                mkInsertPayload row = AutoRefreshRowChangePayload AutoRefreshInsert Nothing (Just (Aeson.Object row)) Nothing
                mkUpdatePayload rowId = AutoRefreshRowChangePayload AutoRefreshUpdate Nothing (Just (Aeson.Object (mkRow [("id", Aeson.String rowId)]))) Nothing
                textParam value = Param (contramap (const value) (Encoders.param (Encoders.nonNullable Encoders.text)))

            it "filters inserts using tracked conditions" $ \_ -> do
                let condition = ColumnCondition "tasks.project_id" EqOp (textParam "abc") Nothing Nothing
                    matchingPayload = mkInsertPayload (mkRow [("project_id", Aeson.String "abc")])
                    otherPayload = mkInsertPayload (mkRow [("project_id", Aeson.String "other")])
                shouldRefreshForPayload mempty (Just [Just (toDyn condition)]) matchingPayload `shouldBe` True
                shouldRefreshForPayload mempty (Just [Just (toDyn condition)]) otherPayload `shouldBe` False

            it "refreshes inserts when no usable condition is tracked" $ \_ -> do
                let payload = mkInsertPayload (mkRow [("id", Aeson.String "1")])
                shouldRefreshForPayload (Set.singleton "1") Nothing payload `shouldBe` True
                shouldRefreshForPayload mempty (Just [Nothing]) payload `shouldBe` True

            it "filters updates using tracked IDs" $ \_ -> do
                shouldRefreshForPayload (Set.singleton "abc-123") Nothing (mkUpdatePayload "abc-123") `shouldBe` True
                shouldRefreshForPayload (Set.singleton "abc-123") Nothing (mkUpdatePayload "other-id") `shouldBe` False
