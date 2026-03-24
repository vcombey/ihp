{-# LANGUAGE OverloadedStrings #-}

module IHP.HSX.Format.Pretty
    ( FormatConfig (..)
    , renderQuoteBody
    ) where

import Prelude
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import IHP.HSX.Format.Types

data FormatConfig = FormatConfig
    { quoteLinePrefix :: !Text
    , indentUnit :: !Text
    , maxWidth :: !Int
    } deriving (Eq, Show)

renderQuoteBody :: FormatConfig -> [QuoteNode] -> Text
renderQuoteBody config nodes = case renderInlineRoot config nodes of
    Just inlineBody -> inlineBody
    Nothing
        | null nodes -> ""
        | otherwise ->
            "\n"
                <> Text.intercalate "\n" (map (renderBlockNode config childIndent) nodes)
                <> "\n"
                <> quoteLinePrefix config
    where
        childIndent = quoteLinePrefix config <> indentUnit config

renderInlineRoot :: FormatConfig -> [QuoteNode] -> Maybe Text
renderInlineRoot config [node] = do
    inlineNode <- renderInlineNode config node
    if Text.length inlineNode <= maxWidth config
        then Just inlineNode
        else Nothing
renderInlineRoot _ _ = Nothing

renderBlockNode :: FormatConfig -> Text -> QuoteNode -> Text
renderBlockNode config currentIndent node = case node of
    TextNode text -> currentIndent <> text
    RawTextNode rawText -> renderRawTextBlock currentIndent rawText
    SpliceNode expression -> renderExpressionBlock config currentIndent expression
    CommentNode body -> renderCommentBlock currentIndent body
    NoRenderCommentNode body -> renderNoRenderCommentBlock currentIndent body
    Element name attributes children closingStyle ->
        renderElementBlock config currentIndent name attributes children closingStyle

renderElementBlock :: FormatConfig -> Text -> Text -> [QuoteAttribute] -> [QuoteNode] -> ClosingStyle -> Text
renderElementBlock config currentIndent name attributes children closingStyle =
    case renderInlineElement config name attributes children closingStyle of
        Just inlineElement -> currentIndent <> inlineElement
        Nothing ->
            case closingStyle of
                VoidLeaf ->
                    renderStartTagBlock config currentIndent name attributes True
                SelfClosing ->
                    renderStartTagBlock config currentIndent name attributes True
                ExplicitClosing ->
                    let openTag = renderStartTagBlock config currentIndent name attributes False
                        nextIndent = currentIndent <> indentUnit config
                        childLines = Text.intercalate "\n" (map (renderBlockNode config nextIndent) children)
                        closeTag = currentIndent <> "</" <> name <> ">"
                    in if null children
                        then openTag <> closeTag
                        else Text.intercalate "\n" [openTag, childLines, closeTag]

renderStartTagBlock :: FormatConfig -> Text -> Text -> [QuoteAttribute] -> Bool -> Text
renderStartTagBlock config currentIndent name attributes isSelfClosing =
    case renderInlineAttributes config attributes of
        Just inlineAttributes
            | Text.length (name <> inlineAttributes) <= maxWidth config - Text.length currentIndent ->
                currentIndent <> "<" <> name <> inlineAttributes <> if isSelfClosing then "/>" else ">"
        _ ->
            let attrIndent = currentIndent <> indentUnit config
                attributeLines = map (renderBlockAttribute config attrIndent) attributes
                closingLine = currentIndent <> if isSelfClosing then "/>" else ">"
            in Text.intercalate "\n" ([currentIndent <> "<" <> name] <> attributeLines <> [closingLine])

renderInlineElement :: FormatConfig -> Text -> [QuoteAttribute] -> [QuoteNode] -> ClosingStyle -> Maybe Text
renderInlineElement config name attributes children closingStyle = do
    if any isStructuralChild children
        then Nothing
        else pure ()
    inlineAttributes <- renderInlineAttributes config attributes
    let openTag = "<" <> name <> inlineAttributes
    case closingStyle of
        VoidLeaf ->
            let candidate = openTag <> "/>"
            in if Text.length candidate <= maxWidth config then Just candidate else Nothing
        SelfClosing ->
            let candidate = openTag <> "/>"
            in if Text.length candidate <= maxWidth config then Just candidate else Nothing
        ExplicitClosing -> do
            inlineChildren <- traverse (renderInlineNode config) children
            let candidate = openTag <> ">" <> Text.concat inlineChildren <> "</" <> name <> ">"
            if Text.length candidate <= maxWidth config
                then Just candidate
                else Nothing

isStructuralChild :: QuoteNode -> Bool
isStructuralChild node = case node of
    Element {} -> True
    RawTextNode {} -> True
    _ -> False

renderInlineNode :: FormatConfig -> QuoteNode -> Maybe Text
renderInlineNode config node = case node of
    TextNode text -> Just text
    RawTextNode rawText
        | isSingleLineText rawText -> Just (Text.strip rawText)
        | otherwise -> Nothing
    SpliceNode expression -> renderInlineExpression expression
    CommentNode body
        | isSingleLineText body -> Just ("<!-- " <> Text.strip body <> " -->")
        | otherwise -> Nothing
    NoRenderCommentNode body
        | isSingleLineText body -> Just ("{- " <> Text.strip body <> " -}")
        | otherwise -> Nothing
    Element name attributes children closingStyle ->
        renderInlineElement config name attributes children closingStyle

renderInlineAttributes :: FormatConfig -> [QuoteAttribute] -> Maybe Text
renderInlineAttributes _ [] = Just ""
renderInlineAttributes config attributes = do
    inlineAttributes <- traverse renderInlineAttribute attributes
    let rendered = " " <> Text.intercalate " " inlineAttributes
    if Text.length rendered <= maxWidth config
        then Just rendered
        else Nothing

renderInlineAttribute :: QuoteAttribute -> Maybe Text
renderInlineAttribute attribute = case attribute of
    BareAttribute name -> Just name
    StaticAttribute name (TextValue value) -> Just (name <> "=\"" <> value <> "\"")
    StaticAttribute name (ExpressionValue expression) -> do
        inlineExpression <- renderInlineExpression expression
        Just (name <> "=" <> inlineExpression)
    SpreadAttribute expression -> do
        inlineExpression <- renderInlineExpression expression
        Just ("{" <> "..." <> stripBraces inlineExpression <> "}")

renderBlockAttribute :: FormatConfig -> Text -> QuoteAttribute -> Text
renderBlockAttribute config currentIndent attribute = case attribute of
    BareAttribute name -> currentIndent <> name
    StaticAttribute name (TextValue value) -> currentIndent <> name <> "=\"" <> value <> "\""
    StaticAttribute name (ExpressionValue expression)
        | isSingleLineText expression -> currentIndent <> name <> "={" <> Text.strip expression <> "}"
        | otherwise ->
            Text.intercalate "\n"
                [ currentIndent <> name <> "={"
                , indentLines (currentIndent <> indentUnit config) (normalizeMultilineBlock expression)
                , currentIndent <> "}"
                ]
    SpreadAttribute expression
        | isSingleLineText expression -> currentIndent <> "{..." <> Text.strip expression <> "}"
        | otherwise ->
            Text.intercalate "\n"
                [ currentIndent <> "{..."
                , indentLines (currentIndent <> indentUnit config) (normalizeMultilineBlock expression)
                , currentIndent <> "}"
                ]

renderInlineExpression :: Text -> Maybe Text
renderInlineExpression expression
    | isSingleLineText expression = Just ("{" <> Text.strip expression <> "}")
    | otherwise = Nothing

renderExpressionBlock :: FormatConfig -> Text -> Text -> Text
renderExpressionBlock config currentIndent expression
    | isSingleLineText expression = currentIndent <> "{" <> Text.strip expression <> "}"
    | otherwise =
        Text.intercalate "\n"
            [ currentIndent <> "{"
            , indentLines (currentIndent <> indentUnit config) (normalizeMultilineBlock expression)
            , currentIndent <> "}"
            ]

renderCommentBlock :: Text -> Text -> Text
renderCommentBlock currentIndent body
    | isSingleLineText body = currentIndent <> "<!-- " <> Text.strip body <> " -->"
    | otherwise =
        Text.intercalate "\n"
            [ currentIndent <> "<!--"
            , indentLines currentIndent (normalizeMultilineBlock body)
            , currentIndent <> "-->"
            ]

renderNoRenderCommentBlock :: Text -> Text -> Text
renderNoRenderCommentBlock currentIndent body
    | isSingleLineText body = currentIndent <> "{- " <> Text.strip body <> " -}"
    | otherwise =
        Text.intercalate "\n"
            [ currentIndent <> "{-"
            , indentLines currentIndent (normalizeMultilineBlock body)
            , currentIndent <> "-}"
            ]

renderRawTextBlock :: Text -> Text -> Text
renderRawTextBlock currentIndent rawText
    | Text.null (normalizeMultilineBlock rawText) = currentIndent
    | otherwise = indentLines currentIndent (normalizeMultilineBlock rawText)

indentLines :: Text -> Text -> Text
indentLines prefix value =
    Text.intercalate "\n" (map (\line -> prefix <> line) (Text.lines value))

normalizeMultilineBlock :: Text -> Text
normalizeMultilineBlock = stripCommonIndent . stripOuterBlankLines

stripOuterBlankLines :: Text -> Text
stripOuterBlankLines =
    Text.unlines
        . reverse
        . dropWhile (Text.all isSpace)
        . reverse
        . dropWhile (Text.all isSpace)
        . Text.lines

stripCommonIndent :: Text -> Text
stripCommonIndent value =
    let lines' = Text.lines value
        indents = map (Text.length . Text.takeWhile isSpace) (filter (not . Text.all isSpace) lines')
    in case indents of
        [] -> Text.strip value
        _ ->
            let smallestIndent = foldl' min (head indents) (tail indents)
            in Text.intercalate "\n" (map (Text.drop smallestIndent) lines')

isSingleLineText :: Text -> Bool
isSingleLineText value = not ("\n" `Text.isInfixOf` Text.strip value)

stripBraces :: Text -> Text
stripBraces value =
    fromMaybe value do
        stripped <- Text.stripPrefix "{" value
        Text.stripSuffix "}" stripped

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback maybeValue = case maybeValue of
    Just value -> value
    Nothing -> fallback
