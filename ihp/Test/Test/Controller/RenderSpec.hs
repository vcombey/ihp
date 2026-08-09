module Test.Controller.RenderSpec where

import IHP.Prelude
import Test.Hspec
import IHP.Controller.Render (renderFragment, respondHtmlFragment)
import IHP.Controller.Response (responseHeadersVaultKey)
import IHP.AutoRefresh (autoRefreshStateVaultKey)
import IHP.AutoRefresh.Types (AutoRefreshState (..))
import IHP.ViewPrelude
import IHP.Test.Mocking (responseBody)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Wai.Request.Params.Middleware (RequestBody (..), Respond, requestBodyVaultKey)

data FragmentView = FragmentView

instance View FragmentView where
    html FragmentView = [hsx|<section id="fragment-content">Hi</section>|]

tests :: Spec
tests = describe "IHP.Controller.Render" do
    describe "respondHtmlFragment" do
        it "prepends autoRefreshMeta when auto refresh is enabled" do
            let sessionId = parseSessionId "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
            request <- buildRequest (Just sessionId)

            response <- captureResponse request do
                respondHtmlFragment [hsx|<div id="fragment">Hello</div>|]

            body :: Text <- cs <$> responseBody response
            let expectedMeta = "<meta property=\"ihp-auto-refresh-id\" content=\"" <> tshow sessionId <> "\">"

            body `shouldSatisfy` Text.isPrefixOf expectedMeta
            body `shouldSatisfy` Text.isInfixOf "<div id=\"fragment\">Hello</div>"

        it "does not prepend autoRefreshMeta when auto refresh is disabled" do
            request <- buildRequest Nothing

            response <- captureResponse request do
                respondHtmlFragment [hsx|<div id="fragment">Hello</div>|]

            body :: Text <- cs <$> responseBody response
            body `shouldSatisfy` Text.isInfixOf "<div id=\"fragment\">Hello</div>"
            body `shouldNotSatisfy` Text.isInfixOf "ihp-auto-refresh-id"

    describe "renderFragment" do
        it "renders the fragment view content" do
            request <- buildRequest Nothing

            response <- captureResponse request do
                renderFragment FragmentView

            body :: Text <- cs <$> responseBody response
            body `shouldSatisfy` Text.isInfixOf "<section id=\"fragment-content\">Hi</section>"

buildRequest :: Maybe UUID -> IO Wai.Request
buildRequest autoRefreshSession = do
    headersRef <- newIORef []

    let withResponseHeaders =
            Vault.insert responseHeadersVaultKey headersRef Vault.empty
    let withRequestBody =
            Vault.insert requestBodyVaultKey (FormBody [] [] "") withResponseHeaders
    let withAutoRefreshState =
            case autoRefreshSession of
                Just sessionId -> Vault.insert autoRefreshStateVaultKey (AutoRefreshEnabled sessionId) withRequestBody
                Nothing -> withRequestBody

    pure Wai.defaultRequest { Wai.vault = withAutoRefreshState }

captureResponse
    :: Wai.Request
    -> ((?context :: Wai.Request, ?request :: Wai.Request, ?respond :: Respond) => IO ResponseReceived)
    -> IO Wai.Response
captureResponse request action = do
    responseRef <- newIORef Nothing
    let ?request = request
    let ?respond = \response -> do
            writeIORef responseRef (Just response)
            pure ResponseReceived
    let ?context = request

    _ <- action
    maybeResponse <- readIORef responseRef
    case maybeResponse of
        Just response -> pure response
        Nothing -> do
            expectationFailure "Expected action to send a response"
            error "unreachable"

parseSessionId :: String -> UUID
parseSessionId value =
    case UUID.fromString value of
        Just sessionId -> sessionId
        Nothing -> error ("Invalid UUID in test: " <> cs value)
