{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.LocalFirstSpec where

import Test.Hspec
import IHP.Prelude
import IHP.LocalFirst
import IHP.LocalFirst.Types
import IHP.LocalFirst.Safety
import IHP.LocalFirst.CodeGen
import IHP.AutoRefresh
import IHP.AutoRefresh.View
import IHP.ControllerSupport
import IHP.Controller.Context
import IHP.Router.UrlGenerator
import qualified Network.Wai as Wai
import qualified Data.Text as Text
import qualified System.IO.Temp as Temp
import qualified Data.Text.IO as Text
import qualified Text.Blaze.Html.Renderer.Text as Blaze

data DemoController = DemoAction deriving (Eq, Show)

instance Controller DemoController where
    action DemoAction = pure ()

instance HasPath DemoController where
    pathTo DemoAction = "/Demo"

tests :: Spec
tests = do
    describe "local" do
        it "marks a request context as local-first" do
            let ?request = Wai.defaultRequest
            context <- newControllerContext
            let ?context = context
            let ?modelContext = error "not needed in this test"
            let ?theAction = DemoAction
            initLocalFirst
            local (pure ())
            localState <- fromContext @LocalFirstState
            localState `shouldSatisfy` \case
                LocalFirstEnabled { routePath = LocalRoutePath "/Demo" } -> True
                _ -> False

        it "injects local metadata through autoRefreshMeta" do
            let ?request = Wai.defaultRequest
            context <- newControllerContext
            let ?context = context
            let ?modelContext = error "not needed in this test"
            let ?theAction = DemoAction
            initAutoRefresh
            initLocalFirst
            local (pure ())
            frozen <- freeze ?context
            let ?context = frozen
            let rendered = cs (Blaze.renderHtml autoRefreshMeta) :: Text
            rendered `shouldSatisfy` Text.isInfixOf "ihp-local-route"

    describe "LocalSafety" do
        it "finds unsafe API usage inside local blocks" do
            let source = Text.unlines
                    [ "action DemoAction = local do"
                    , "    response <- httpLBS request"
                    , "    pure ()"
                    ]
            let violations = scanLocalSafetySource "Web/Controller/Demo.hs" source
            length violations `shouldBe` 1

    describe "LocalFirst.CodeGen" do
        it "discovers local actions from source files" do
            Temp.withSystemTempDirectory "ihp-localfirst" \tmpDir -> do
                let sourcePath = tmpDir <> "/Demo.hs"
                Text.writeFile sourcePath (Text.unlines
                    [ "module Web.Controller.Demo where"
                    , "action DemoAction = local do"
                    , "    pure ()"
                    ])
                routes <- discoverLocalRoutesInFile sourcePath
                routes `shouldBe` [LocalRouteDefinition
                    { moduleName = "Web.Controller.Demo"
                    , actionName = "DemoAction"
                    , sourceFile = sourcePath <> ":2"
                    }]

        it "discovers transpileable local update actions" do
            Temp.withSystemTempDirectory "ihp-localfirst" \tmpDir -> do
                let sourcePath = tmpDir <> "/Todos.hs"
                Text.writeFile sourcePath (Text.unlines
                    [ "module Web.Controller.Todos where"
                    , "action UpdateTodoAction { todoId } = local do"
                    , "    todo <- fetch todoId"
                    , "    let title = param @Text \"title\""
                    , "    let isCompleted = paramOrDefault False \"isCompleted\""
                    , "    todo"
                    , "        |> set #title title"
                    , "        |> set #isCompleted isCompleted"
                    , "        |> updateRecord"
                    , "    redirectTo TodosAction"
                    ])
                actions <- discoverGeneratedLocalActionsInFile sourcePath
                actions `shouldBe`
                    [ LocalGeneratedUpdateAction
                        { actionName = "UpdateTodoAction"
                        , routePath = "/UpdateTodo"
                        , tableName = "todos"
                        , idField = "todoId"
                        , fields =
                            [ ("title", "title", LocalFieldText)
                            , ("isCompleted", "isCompleted", LocalFieldBool)
                            ]
                        }
                    ]

        it "discovers transpileable local create actions" do
            Temp.withSystemTempDirectory "ihp-localfirst" \tmpDir -> do
                let sourcePath = tmpDir <> "/Todos.hs"
                Text.writeFile sourcePath (Text.unlines
                    [ "module Web.Controller.Todos where"
                    , "action CreateTodoAction = local do"
                    , "    let title = paramOrDefault \"\" \"title\""
                    , "    let isCompleted = paramOrDefault False \"isCompleted\""
                    , "    let todo = newRecord @Todo"
                    , "    todo"
                    , "        |> set #title title"
                    , "        |> set #isCompleted isCompleted"
                    , "        |> createRecord"
                    , "    redirectTo TodosAction"
                    ])
                actions <- discoverGeneratedLocalActionsInFile sourcePath
                actions `shouldBe`
                    [ LocalGeneratedCreateAction
                        { actionName = "CreateTodoAction"
                        , routePath = "/CreateTodo"
                        , tableName = "todos"
                        , fields =
                            [ ("title", "title", LocalFieldText)
                            , ("isCompleted", "isCompleted", LocalFieldBool)
                            ]
                        }
                    ]

        it "renders generated local route script with automatic handler registration" do
            let script = renderGeneratedLocalRoutesScript
                    [ LocalGeneratedUpdateAction
                        { actionName = "UpdateTodoAction"
                        , routePath = "/UpdateTodo"
                        , tableName = "todos"
                        , idField = "todoId"
                        , fields =
                            [ ("title", "title", LocalFieldText)
                            , ("isCompleted", "isCompleted", LocalFieldBool)
                            ]
                        }
                    , LocalGeneratedCreateAction
                        { actionName = "CreateTodoAction"
                        , routePath = "/CreateTodo"
                        , tableName = "todos"
                        , fields =
                            [ ("title", "title", LocalFieldText)
                            , ("isCompleted", "isCompleted", LocalFieldBool)
                            ]
                        }
                    ]
            script `shouldSatisfy` Text.isInfixOf "registerAction('/UpdateTodo'"
            script `shouldSatisfy` Text.isInfixOf "updateRecord('todos'"
            script `shouldSatisfy` Text.isInfixOf "is_completed"
            script `shouldSatisfy` Text.isInfixOf "registerAction('/CreateTodo'"
            script `shouldSatisfy` Text.isInfixOf "createRecord('todos'"
