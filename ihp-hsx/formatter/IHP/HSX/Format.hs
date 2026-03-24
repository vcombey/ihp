{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module IHP.HSX.Format
    ( Backend (..)
    , FormatOptions (..)
    , defaultFormatOptions
    , formatSource
    ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified IHP.HSX.Format.Backend as Backend
import IHP.HSX.Format.Backend (Backend (..))
import IHP.HSX.Format.Locate
import IHP.HSX.Format.Parser
import IHP.HSX.Format.Pretty
import IHP.HSX.Format.Types
import qualified Text.Megaparsec as Megaparsec

data FormatOptions = FormatOptions
    { backend :: !Backend
    , sourcePath :: !FilePath
    , maxWidth :: !Int
    } deriving (Eq, Show)

defaultFormatOptions :: FilePath -> FormatOptions
defaultFormatOptions sourcePath =
    FormatOptions
        { backend = NoBackend
        , sourcePath
        , maxWidth = 100
        }

formatSource :: FormatOptions -> Text -> IO (Either Text Text)
formatSource options input = do
    let pragmas = collectLanguagePragmas input
    rewritten <- rewriteQuotesToFixedPoint options pragmas input 0
    case rewritten of
        Left errorMessage -> pure (Left errorMessage)
        Right stableSource ->
            Backend.runSourceBackend options.backend options.sourcePath stableSource

rewriteQuotesToFixedPoint :: FormatOptions -> [Text] -> Text -> Int -> IO (Either Text Text)
rewriteQuotesToFixedPoint options pragmas input iteration = do
    rewritten <- rewriteQuotes options pragmas input
    case rewritten of
        Left errorMessage -> pure (Left errorMessage)
        Right output
            | output == input -> pure (Right output)
            | iteration >= 3 -> pure (Right output)
            | otherwise -> rewriteQuotesToFixedPoint options pragmas output (iteration + 1)

rewriteQuotes :: FormatOptions -> [Text] -> Text -> IO (Either Text Text)
rewriteQuotes options pragmas input = do
    formattedQuotes <- traverse (formatQuote options pragmas input) (locateQuotes input)
    pure do
        replacements <- sequence formattedQuotes
        Right (applyReplacements input replacements)

formatQuote :: FormatOptions -> [Text] -> Text -> QuoteRegion -> IO (Either Text (QuoteRegion, Text))
formatQuote options pragmas input quoteRegion@QuoteRegion { bodyStartIndex, bodyEndIndex, linePrefix } = do
    let body = slice bodyStartIndex bodyEndIndex input
    case parseQuoteBody body of
        Left parseError ->
            pure (Left ("Unable to parse HSX quasiquote:\n" <> Text.pack (Megaparsec.errorBundlePretty parseError)))
        Right parsedNodes -> do
            formattedNodes <- formatExpressions options pragmas parsedNodes
            pure $ do
                nodesWithFormattedExpressions <- formattedNodes
                let renderedBody = renderQuoteBody
                        FormatConfig
                            { quoteLinePrefix = linePrefix
                            , indentUnit = "    "
                            , maxWidth = options.maxWidth
                            }
                        nodesWithFormattedExpressions
                Right (quoteRegion, renderedBody)

formatExpressions :: FormatOptions -> [Text] -> [QuoteNode] -> IO (Either Text [QuoteNode])
formatExpressions options pragmas nodes = do
    formattedNodes <- traverse (formatNodeExpressions options pragmas) nodes
    pure (sequence formattedNodes)

formatNodeExpressions :: FormatOptions -> [Text] -> QuoteNode -> IO (Either Text QuoteNode)
formatNodeExpressions options pragmas node = case node of
    Element name attributes children closingStyle -> do
        formattedAttributes <- traverse (formatAttributeExpressions options pragmas) attributes
        formattedChildren <- traverse (formatNodeExpressions options pragmas) children
        pure do
            attrs <- sequence formattedAttributes
            childNodes <- sequence formattedChildren
            Right (Element name attrs childNodes closingStyle)
    SpliceNode expression -> do
        formattedExpression <- Backend.formatExpressionWithBackend options.backend options.sourcePath pragmas expression
        pure (SpliceNode <$> formattedExpression)
    TextNode _ -> pure (Right node)
    RawTextNode _ -> pure (Right node)
    CommentNode _ -> pure (Right node)
    NoRenderCommentNode _ -> pure (Right node)

formatAttributeExpressions :: FormatOptions -> [Text] -> QuoteAttribute -> IO (Either Text QuoteAttribute)
formatAttributeExpressions options pragmas attribute = case attribute of
    BareAttribute _ -> pure (Right attribute)
    StaticAttribute name (TextValue value) -> pure (Right (StaticAttribute name (TextValue value)))
    StaticAttribute name (ExpressionValue expression) -> do
        formattedExpression <- Backend.formatExpressionWithBackend options.backend options.sourcePath pragmas expression
        pure (StaticAttribute name . ExpressionValue <$> formattedExpression)
    SpreadAttribute expression -> do
        formattedExpression <- Backend.formatExpressionWithBackend options.backend options.sourcePath pragmas expression
        pure (SpreadAttribute <$> formattedExpression)

applyReplacements :: Text -> [(QuoteRegion, Text)] -> Text
applyReplacements input replacements = go 0 replacements
    where
        go start [] = Text.drop start input
        go start ((quoteRegion, body):rest) =
            slice start (bodyStartIndex quoteRegion) input
                <> body
                <> go (bodyEndIndex quoteRegion) rest

slice :: Int -> Int -> Text -> Text
slice start end = Text.take (end - start) . Text.drop start

collectLanguagePragmas :: Text -> [Text]
collectLanguagePragmas input =
    [ line
    | line <- Text.lines input
    , "{-# LANGUAGE " `Text.isPrefixOf` Text.stripStart line
    ]
