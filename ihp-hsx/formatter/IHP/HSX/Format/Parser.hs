{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module IHP.HSX.Format.Parser
    ( parseQuoteBody
    ) where

import Prelude
import Data.Char (isAlphaNum, isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import IHP.HSX.Format.Types
import IHP.HSX.Parser (collapseSpace)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void Text

parseQuoteBody :: Text -> Either (ParseErrorBundle Text Void) [QuoteNode]
parseQuoteBody input = runParser parser "" input

parser :: Parser [QuoteNode]
parser = do
    space
    nodes <- many hsxChild
    space
    eof
    pure (stripTextNodeWhitespaces nodes)

hsxChild :: Parser QuoteNode
hsxChild = do
    c <- lookAhead anySingle
    case c of
        '<' -> hsxOpenTag
        '{' -> hsxNoRenderCommentOrSplice
        _ | isSpace c -> hsxSpaceThenChild
          | otherwise -> hsxText

hsxOpenTag :: Parser QuoteNode
hsxOpenTag = do
    _ <- char '<'
    hsxCommentContent <|> hsxTagContent

hsxCommentContent :: Parser QuoteNode
hsxCommentContent = do
    _ <- string "!--"
    body <- scanUntil "-->"
    space
    pure (CommentNode body)

hsxTagContent :: Parser QuoteNode
hsxTagContent = do
    name <- hsxElementName
    attributes <- hsxAttributes
    let isVoid = name `Set.member` voidTags
    (string "/>" >> space >> pure (Element name attributes [] (if isVoid then VoidLeaf else SelfClosing)))
        <|> (char '>' >> if isVoid
                then space >> pure (Element name attributes [] VoidLeaf)
                else hsxTagChildren name attributes)

hsxTagChildren :: Text -> [QuoteAttribute] -> Parser QuoteNode
hsxTagChildren name attributes = do
    children <- case name of
        "script" -> parseRawTextChildren name Text.strip
        "style" -> parseRawTextChildren name Text.strip
        _ -> space >> collectChildren name []
    pure (Element name attributes children ExplicitClosing)

parseRawTextChildren :: Text -> (Text -> Text) -> Parser [QuoteNode]
parseRawTextChildren name transformText = do
    body <- scanUntil ("</" <> name <> ">")
    pure [RawTextNode (transformText body)]

collectChildren :: Text -> [QuoteNode] -> Parser [QuoteNode]
collectChildren name !acc = do
    input <- getInput
    if Text.isPrefixOf "</" (Text.dropWhile isSpace input)
        then do
            space
            hsxClosingElement name
            pure (stripTextNodeWhitespaces (reverse acc))
        else do
            child <- hsxChild
            collectChildren name (child : acc)

hsxClosingElement :: Text -> Parser ()
hsxClosingElement name = do
    _ <- string ("</" <> name)
    space
    _ <- char '>'
    pure ()

hsxAttributes :: Parser [QuoteAttribute]
hsxAttributes = do
    attributes <- many (try (space1 *> (hsxSpreadAttribute <|> hsxNodeAttribute)))
    space
    pure attributes

hsxSpreadAttribute :: Parser QuoteAttribute
hsxSpreadAttribute = do
    _ <- char '{'
    space
    _ <- string "..."
    space
    value <- scanBracedExpression 0 []
    pure (SpreadAttribute (Text.strip value))

hsxNodeAttribute :: Parser QuoteAttribute
hsxNodeAttribute = do
    key <- hsxAttributeName
    space
    optionalValue <- optional do
        _ <- char '='
        space
        hsxQuotedValue <|> hsxSplicedValue
    pure $ case optionalValue of
        Nothing -> BareAttribute key
        Just value -> StaticAttribute key value

hsxQuotedValue :: Parser QuoteAttributeValue
hsxQuotedValue = do
    value <- between (char '"') (char '"') (takeWhileP Nothing (/= '"'))
    pure (TextValue value)

hsxSplicedValue :: Parser QuoteAttributeValue
hsxSplicedValue = do
    _ <- char '{'
    value <- scanBracedExpression 0 []
    pure (ExpressionValue (Text.strip value))

hsxNoRenderCommentOrSplice :: Parser QuoteNode
hsxNoRenderCommentOrSplice = do
    mc <- optional (lookAhead (string "{-"))
    case mc of
        Just _ -> hsxNoRenderComment
        Nothing -> hsxSplicedNode

hsxNoRenderComment :: Parser QuoteNode
hsxNoRenderComment = do
    _ <- string "{-"
    body <- scanUntil "-}"
    space
    pure (NoRenderCommentNode body)

hsxSpaceThenChild :: Parser QuoteNode
hsxSpaceThenChild = do
    sp <- takeWhileP Nothing isSpace
    mc <- optional (lookAhead anySingle)
    case mc of
        Nothing -> pure (buildTextNode sp)
        Just '<' -> hsxOpenTag
        Just '{' -> do
            mc2 <- optional (lookAhead (string "{-"))
            case mc2 of
                Just _ -> hsxNoRenderComment
                Nothing -> pure (buildTextNode sp)
        Just _ -> do
            rest <- takeWhileP Nothing (\c -> c /= '{' && c /= '}' && c /= '<' && c /= '>')
            pure (buildTextNode (sp <> rest))

hsxText :: Parser QuoteNode
hsxText = buildTextNode <$> takeWhile1P (Just "text") (\c -> c /= '{' && c /= '}' && c /= '<' && c /= '>')

buildTextNode :: Text -> QuoteNode
buildTextNode value = TextNode (collapseSpace value)

hsxSplicedNode :: Parser QuoteNode
hsxSplicedNode = do
    _ <- char '{'
    expression <- scanBracedExpression 0 []
    pure (SpliceNode (Text.strip expression))

scanBracedExpression :: Int -> [Text] -> Parser Text
scanBracedExpression !depth !acc = do
    chunk <- takeWhileP Nothing (\c -> c /= '{' && c /= '}')
    let acc' = if Text.null chunk then acc else chunk : acc
    c <- anySingle
    case c of
        '{' -> scanBracedExpression (depth + 1) ("{" : acc')
        '}' | depth > 0 -> scanBracedExpression (depth - 1) ("}" : acc')
            | otherwise -> pure (Text.concat (reverse acc'))
        _ -> error "scanBracedExpression: unreachable branch"

scanUntil :: Text -> Parser Text
scanUntil closingTag = Text.concat . reverse <$> go []
    where
        firstChar = Text.head closingTag
        go acc = do
            chunk <- takeWhileP Nothing (/= firstChar)
            let acc' = chunk : acc
            (string closingTag *> pure acc') <|> do
                c <- anySingle
                go (Text.singleton c : acc')

hsxElementName :: Parser Text
hsxElementName = do
    name <- takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '_' || c == '-' || c == '!')
    pure name

hsxAttributeName :: Parser Text
hsxAttributeName = do
    name <- takeWhile1P Nothing (\c -> isAlphaNum c || c == '-' || c == '_' || c == ':')
    pure name

stripTextNodeWhitespaces :: [QuoteNode] -> [QuoteNode]
stripTextNodeWhitespaces = stripLastTextNodeWhitespaces . stripFirstTextNodeWhitespaces

stripLastTextNodeWhitespaces :: [QuoteNode] -> [QuoteNode]
stripLastTextNodeWhitespaces [] = []
stripLastTextNodeWhitespaces [TextNode text] = [TextNode (Text.stripEnd text)]
stripLastTextNodeWhitespaces [node] = [node]
stripLastTextNodeWhitespaces (x:xs) = x : stripLastTextNodeWhitespaces xs

stripFirstTextNodeWhitespaces :: [QuoteNode] -> [QuoteNode]
stripFirstTextNodeWhitespaces [] = []
stripFirstTextNodeWhitespaces (TextNode text : rest) = TextNode (Text.stripStart text) : rest
stripFirstTextNodeWhitespaces nodes = nodes

voidTags :: Set Text
voidTags = Set.fromList
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "!DOCTYPE"
    ]
