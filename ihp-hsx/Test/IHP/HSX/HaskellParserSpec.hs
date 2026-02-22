module IHP.HSX.HaskellParserSpec where

import Prelude
import Test.Hspec
import Control.Exception (SomeException, displayException, evaluate, try)
import Data.List (isInfixOf)
import qualified "template-haskell" Language.Haskell.TH as TH
import qualified Text.Megaparsec as Megaparsec
import IHP.HSX.HaskellParser (mkHaskellExprParser, parseHaskellExpression)
import qualified IHP.HSX.QQ as Blaze

tests :: SpecWith ()
tests = describe "HSX haskell splice parser errors" do
    it "parses valid quoteExp hsx splices in expressions" do
        result <- runParseAndForce "if True then $(quoteExp hsx \"<span>A</span>\") else $(quoteExp hsx \"<span>B</span>\")"
        case result of
            Left err -> expectationFailure ("Expected successful parse, but got exception: " <> displayException err)
            Right parseResult ->
                case parseResult of
                    Left parseError -> expectationFailure ("Expected successful parse, but got: " <> show parseError)
                    Right _ -> pure ()

    it "parses qualified quoteExp references in expressions" do
        result <- runParseAndForce "$(Language.Haskell.TH.Quote.quoteExp hsx \"<span>A</span>\")"
        case result of
            Left err -> expectationFailure ("Expected successful parse, but got exception: " <> displayException err)
            Right parseResult ->
                case parseResult of
                    Left parseError -> expectationFailure ("Expected successful parse, but got: " <> show parseError)
                    Right _ -> pure ()

    it "reports malformed nested HSX diagnostics without internal converter errors" do
        let expression = "if True then $(quoteExp hsx \"<dvi>Admin</dvi>\") else $(quoteExp hsx \"<span>User</span>\")"
        result <- runParseAndForce expression
        case result of
            Left err -> do
                let message = displayException err
                message `shouldSatisfy` (\m -> "Invalid tag name: dvi" `isInfixOf` m || "Q monad failure" `isInfixOf` m)
                message `shouldNotContain` "not implemented"
                message `shouldNotContain` "no TemplateHaskell"
            Right parsedResult ->
                case parsedResult of
                    Left (_, _, message) -> message `shouldContain` "Invalid tag name: dvi"
                    Right _ -> expectationFailure "Expected malformed nested HSX to fail"

    it "returns parse errors for malformed haskell expressions" do
        result <- runParse "if True then"
        case result of
            Left (line, _, message) -> do
                line `shouldBe` 1
                message `shouldNotBe` ""
            Right _ -> expectationFailure "Expected malformed haskell expression to fail"

    it "fails with explicit message for unsupported quoteExp targets" do
        let expression = "$(quoteExp nope \"<span>A</span>\")"
        result <- runParseAndForce expression
        case result of
            Left err -> do
                let message = displayException err
                message `shouldContain` "unsupported quoteExp target: nope"
                message `shouldNotContain` "not implemented"
            Right parsedResult ->
                case parsedResult of
                    Left (_, _, message) -> message `shouldContain` "unsupported quoteExp target: nope"
                    Right _ -> expectationFailure "Expected unsupported quoteExp target to fail"

    it "fails with explicit message for unsupported quasiquotes" do
        let expression = "[nope|<span>A</span>|]"
        result <- runParseAndForce expression
        case result of
            Left err -> do
                let message = displayException err
                message `shouldContain` "unsupported quasiquote: nope"
                message `shouldNotContain` "not implemented"
            Right parsedResult ->
                case parsedResult of
                    Left (_, _, message) -> message `shouldContain` "unsupported quasiquote: nope"
                    Right _ -> expectationFailure "Expected unsupported quasiquote to fail"

runParse :: String -> IO (Either (Int, Int, String) TH.Exp)
runParse expression = evaluate (parseHaskellExpression parser sourcePos expression)
  where
    parser = mkHaskellExprParser [] Blaze.expandHsxQuasiQuote
    sourcePos = Megaparsec.SourcePos "<splice>" (Megaparsec.mkPos 1) (Megaparsec.mkPos 1)

runParseAndForce :: String -> IO (Either SomeException (Either (Int, Int, String) TH.Exp))
runParseAndForce expression = try do
    parseResult <- runParse expression
    case parseResult of
        Right parsedExpression -> do
            _ <- evaluate (length (show parsedExpression))
            pure ()
        Left _ -> pure ()
    pure parseResult
