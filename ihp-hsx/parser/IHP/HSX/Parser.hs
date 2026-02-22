{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies  #-}
{-|
Module: IHP.HSX.Parser
Description: Parser for HSX code
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.HSX.Parser
( parseHsx
, Node (..)
, Attribute (..)
, AttributeValue (..)
, renderStaticNode
, collapseSpace
, HsxSettings (..)
) where

import Prelude
import Data.Text (Text)
import Data.Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.String.Conversions
import qualified Data.List as List
import Data.List (sortOn)
import Control.Monad (unless, when)
import qualified "template-haskell" Language.Haskell.TH.Syntax as Haskell
import qualified "template-haskell" Language.Haskell.TH as TH
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified IHP.HSX.HaskellParser as HaskellParser
import IHP.HSX.HaskellParser (HaskellExprParser, mkHaskellExprParser)

data HsxSettings = HsxSettings
    { checkMarkup :: Bool
    , additionalTagNames :: Set Text
    , additionalAttributeNames :: Set Text
    , expandQuasiQuote :: String -> String -> Maybe Haskell.Exp
    }

data AttributeValue = TextValue !Text | ExpressionValue !Haskell.Exp deriving (Eq, Show)

data Attribute = StaticAttribute !Text !AttributeValue | SpreadAttributes !Haskell.Exp deriving (Eq, Show)

data Node = Node !Text ![Attribute] ![Node] !Bool
    | TextNode !Text
    | PreEscapedTextNode !Text -- ^ Used in @script@ or @style@ bodies
    | SplicedNode !Haskell.Exp -- ^ Inline haskell expressions like @{myVar}@ or @{f "hello"}@
    | FragmentNode ![Node] -- ^ Fragment syntax like @<> ... </>@
    | Children ![Node]
    | CommentNode !Text -- ^ A Comment that is rendered in the final HTML
    | NoRenderCommentNode -- ^ A comment that is not rendered in the final HTML
    deriving (Eq, Show)

-- | Parses a HSX text and returns a 'Node'
--
-- __Example:__
--
-- > let filePath = "my-template"
-- > let line = 0
-- > let col = 0
-- > let position = Megaparsec.SourcePos filePath (Megaparsec.mkPos line) (Megaparsec.mkPos col)
-- > let hsxText = "<strong>Hello</strong>"
-- >
-- > let (Right node) = parseHsx settings position [] hsxText
parseHsx :: HsxSettings -> SourcePos -> [TH.Extension] -> Text -> Either (ParseErrorBundle Text Void) Node
parseHsx settings position extensions code =
    let
        ?haskellParser = mkHaskellExprParser extensions settings.expandQuasiQuote
        ?settings = settings
    in
        runParser (setPosition position *> parser) "" code

type Parser a = (?haskellParser :: HaskellExprParser, ?settings :: HsxSettings) => Parsec Void Text a

setPosition pstateSourcePos = updateParserState (\state -> state {
        statePosState = (statePosState state) { pstateSourcePos }
    })

parser :: Parser Node
parser = do
    space
    node <- manyHsxElement <|> hsxOpenTag
    space
    eof
    pure node

manyHsxElement :: Parser Node
manyHsxElement = do
    children <- many hsxChild
    pure (Children (stripTextNodeWhitespaces children))

-- | Parses any element starting with @<@ (comments or tags).
-- After consuming @<@, dispatches to comment or tag parser without backtracking.
hsxOpenTag :: Parser Node
hsxOpenTag = do
    _ <- char '<'
    hsxCommentContent <|> hsxTagContent

-- | Parses an HTML comment after the opening @<@ has been consumed.
hsxCommentContent :: Parser Node
hsxCommentContent = do
    _ <- string "!--"
    body <- hsxCommentBody
    space
    pure (CommentNode body)

-- | Scans for @-->@ producing Text directly (avoids intermediate String).
hsxCommentBody :: Parser Text
hsxCommentBody = Text.concat . reverse <$> go []
    where
        go acc = do
            chunk <- takeWhileP Nothing (/= '-')
            let acc' = chunk : acc
            (string "-->" *> pure acc') <|> do
                c <- anySingle
                go (Text.singleton c : acc')

hsxNoRenderComment :: Parser Node
hsxNoRenderComment = do
    string "{-"
    skipUntil "-}"
    space
    pure NoRenderCommentNode
    where
        skipUntil end = do
            _ <- takeWhileP Nothing (/= Text.head end)
            (string end *> pure ()) <|> (anySingle *> skipUntil end)

-- | Parses a tag element after the opening @<@ has been consumed.
-- Handles both self-closing (@/>@) and normal elements (@>...children...</tag>@)
-- in a single pass, avoiding backtracking over the tag name and attributes.
hsxTagContent :: Parser Node
hsxTagContent = do
    -- React-like fragment syntax: <> ... </>
    (char '>' >> hsxFragmentChildren)
        <|> do
            name <- hsxElementName
            let isLeaf = (Text.toCaseFold name) `Set.member` leafsCI
            attributes <- hsxAttributes
            -- Determine self-closing vs opening tag from the closing delimiter
            (string "/>" >> space >> pure (Node name attributes [] isLeaf))
                <|> (char '>' >> if isLeaf
                        then space >> pure (Node name attributes [] True)
                        else hsxTagChildren name attributes)

hsxFragmentChildren :: Parser Node
hsxFragmentChildren = do
    space
    children <- collectFragmentChildren []
    pure (FragmentNode children)

-- | Parses the children and closing tag of a normal (non-self-closing) element.
hsxTagChildren :: Text -> [Attribute] -> Parser Node
hsxTagChildren name attributes = do
    -- script and style tags have special handling for their children. Inside those tags
    -- we allow any kind of content. Using a haskell expression like @<script>{myHaskellExpr}</script>@
    -- will just literally output the string @{myHaskellExpr}@ without evaluating the haskell expression itself.
    --
    -- Here is an example HSX code explaining the problem:
    -- [hsx|<style>h1 { color: red; }</style>|]
    -- The @{ color: red; }@ would be parsed as a inline haskell expression without the special handling
    --
    -- Additionally we don't do the usual escaping for style and script bodies, as this will make e.g. the
    -- javascript unusuable.
    children <- case Text.toCaseFold name of
        "script" -> parsePreEscapedTextChildren name Text.strip
        "style" -> parsePreEscapedTextChildren name (collapseSpace . Text.strip)
        _ -> space >> collectChildren name []
    pure (Node name attributes children False)

-- | Collect children until closing tag is found.
-- Uses 'getInput' to peek for @<\/@ without backtracking, eliminating two @try@ wrappers per child.
collectChildren :: Text -> [Node] -> Parser [Node]
collectChildren name !acc = do
    input <- getInput
    if Text.isPrefixOf "</" (Text.dropWhile Char.isSpace input)
        then do
            space
            hsxClosingElement name
            pure (stripTextNodeWhitespaces (reverse acc))
        else do
            child <- hsxChild
            collectChildren name (child : acc)

collectFragmentChildren :: [Node] -> Parser [Node]
collectFragmentChildren !acc = do
    input <- getInput
    if Text.isPrefixOf "</>" (Text.dropWhile Char.isSpace input)
        then do
            space
            hsxClosingFragment
            pure (stripTextNodeWhitespaces (reverse acc))
        else do
            child <- hsxChild
            collectFragmentChildren (child : acc)

-- | Parses pre-escaped text (for script/style bodies) by scanning for the closing tag.
-- Produces Text directly without an intermediate String allocation.
parsePreEscapedTextChildren :: Text -> (Text -> Text) -> Parser [Node]
parsePreEscapedTextChildren name transformText = do
    body <- scanUntilTag ("</" <> name <> ">")
    pure [PreEscapedTextNode (transformText body)]

-- | Efficiently scans for a closing tag, producing Text directly.
scanUntilTag :: Text -> Parser Text
scanUntilTag closingTag = Text.concat . reverse <$> go []
    where
        firstChar = Text.head closingTag
        go acc = do
            chunk <- takeWhileP Nothing (/= firstChar)
            let acc' = chunk : acc
            (string closingTag *> pure acc') <|> do
                c <- anySingle
                go (Text.singleton c : acc')

-- | Parses tag attributes until a non-attribute token (like @>@ or @/>@) is encountered.
-- Uses 'many' instead of 'manyTill' since attribute parsers naturally fail
-- on non-identifier characters like @>@ and @/@.
hsxAttributes :: Parser [Attribute]
hsxAttributes = do
    attributes <- many (hsxNodeAttribute <|> hsxSplicedAttributes)
    case findDuplicateKey Set.empty attributes of
        Just dupKey -> fail $ "Duplicate attribute found in tag: " <> show dupKey
        Nothing -> pure attributes
    where
        findDuplicateKey :: Set Text -> [Attribute] -> Maybe Text
        findDuplicateKey !seen [] = Nothing
        findDuplicateKey !seen (StaticAttribute name _ : rest)
            | name `Set.member` seen = Just name
            | otherwise = findDuplicateKey (Set.insert name seen) rest
        findDuplicateKey !seen (_ : rest) = findDuplicateKey seen rest

hsxSplicedAttributes :: Parser Attribute
hsxSplicedAttributes = do
    (pos, expression) <- hsxSplicedExpression
    spreadExpression <- case Text.stripPrefix "..." (Text.stripStart expression) of
        Just value -> pure (Text.strip value)
        Nothing -> fail "Expected spread attributes syntax: {...expression}"
    when (Text.null spreadExpression) do
        fail "Expected expression after `...` in spread attributes"
    space
    haskellExpression <- parseHaskellExpression pos (cs spreadExpression)
    pure (SpreadAttributes haskellExpression)

hsxSplicedExpression :: Parser (SourcePos, Text)
hsxSplicedExpression = do
    _ <- char '{'
    pos <- getSourcePos
    input <- getInput
    case consumeSplicedExpression input of
        Left err -> fail err
        Right (expression, consumedTokens) -> do
            _ <- takeP (Just "spliced expression") consumedTokens
            pure (pos, expression)

parseHaskellExpression :: SourcePos -> Text -> Parser Haskell.Exp
parseHaskellExpression sourcePos input = do
    case HaskellParser.parseHaskellExpression ?haskellParser sourcePos (cs input) of
        Right expression -> pure expression
        Left (line, col, error) -> do
            pos <- getSourcePos
            setPosition pos { sourceLine = mkPos line, sourceColumn = mkPos col }
            fail (show error)

hsxNodeAttribute :: Parser Attribute
hsxNodeAttribute = do
    key <- hsxAttributeName
    space

    -- Boolean attributes like <input disabled/> will be represented as <input disabled="disabled"/>
    -- as there is currently no other way to represent them with blaze-html.
    --
    -- There's a special case for data attributes: Data attributes like <form data-disable-javascript-submission/> will be represented as <form data-disable-javascript-submission="true"/>
    --
    -- See: https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes
    let attributeWithoutValue = do
            let value = if "data-" `Text.isPrefixOf` key
                    then "true"
                    else key
            pure (StaticAttribute key (TextValue value))

    -- Parsing normal attributes like <input value="Hello"/>
    let attributeWithValue = do
            _ <- char '='
            space
            value <- hsxQuotedValue <|> hsxSplicedValue
            space
            pure (StaticAttribute key value)

    attributeWithValue <|> attributeWithoutValue


hsxAttributeName :: Parser Text
hsxAttributeName = do
        name <- rawAttribute
        let checkingMarkup = ?settings.checkMarkup
        unless (isValidAttributeName name || not checkingMarkup) do
            let allAttrs = attributes `Set.union` ?settings.additionalAttributeNames
            let suggestions = findSuggestions name allAttrs
            let prefixHint = missingPrefixHint name
            fail $ "Invalid attribute name: " <> cs name <> cs (formatSuggestions suggestions) <> cs prefixHint
        pure name
    where
        isValidAttributeName name =
            "data-" `Text.isPrefixOf` name
            || "aria-" `Text.isPrefixOf` name
            || "hx-" `Text.isPrefixOf` name
            -- "_" is a valid HTML attribute name per the WHATWG spec, and is useful
            -- syntax for inline scripting libraries such as hyperscript
            || name == "_"
            || name `Set.member` attributes
            || name `Set.member` ?settings.additionalAttributeNames

        -- '_' is included as it is a valid HTML attribute name character, needed
        -- to parse the "_" attribute used by inline scripting libraries such as hyperscript
        rawAttribute = takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-' || c == '_' || c == ':')


hsxQuotedValue :: Parser AttributeValue
hsxQuotedValue = do
    value <- between (char '"') (char '"') (takeWhileP Nothing (\c -> c /= '\"'))
    pure (TextValue value)

hsxSplicedValue :: Parser AttributeValue
hsxSplicedValue = do
    (pos, expression) <- hsxSplicedExpression
    haskellExpression <- parseHaskellExpression pos (cs expression)
    pure (ExpressionValue haskellExpression)

hsxClosingElement name = (hsxClosingElement' name) <?> friendlyErrorMessage
    where
        friendlyErrorMessage = "closing tag </" <> Text.unpack name <> "> (to match opening <" <> Text.unpack name <> "> tag)"
        hsxClosingElement' name = do
            _ <- string ("</" <> name)
            space
            char ('>')
            pure ()

hsxClosingFragment :: Parser ()
hsxClosingFragment = (string "</>" >> space >> pure ()) <?> "closing fragment </> (to match opening <> fragment)"

-- | Dispatch on first character to avoid sequential backtracking.
hsxChild :: Parser Node
hsxChild = do
    c <- lookAhead anySingle
    case c of
        '<' -> hsxOpenTag
        '{' -> hsxNoRenderCommentOrSplice
        _ | Char.isSpace c -> hsxSpaceThenChild
          | otherwise -> hsxText

-- | Peek at second char to distinguish @{- comment -}@ from @{expr}@ splice.
hsxNoRenderCommentOrSplice :: Parser Node
hsxNoRenderCommentOrSplice = do
    mc <- optional (lookAhead (string "{-"))
    case mc of
        Just _  -> hsxNoRenderComment
        Nothing -> hsxSplicedNode

-- | Whitespace in child position: consume it, then dispatch.
-- Space before element/comment is discarded (matches old @try (space >> hsxElement)@ behavior).
-- Space before splice/text/eof becomes a text node (matches old @hsxText@ fallback).
hsxSpaceThenChild :: Parser Node
hsxSpaceThenChild = do
    sp <- takeWhileP Nothing Char.isSpace
    mc <- optional (lookAhead anySingle)
    case mc of
        Nothing -> pure (buildTextNode sp)           -- eof: text node (stripped later)
        Just '<' -> hsxOpenTag                       -- element: discard space
        Just '{' -> do
            mc2 <- optional (lookAhead (string "{-"))
            case mc2 of
                Just _  -> hsxNoRenderComment        -- {- comment: discard space
                Nothing -> pure (buildTextNode sp)   -- {expr}: space is text node
        Just _ -> do                                 -- more text: combine space + content
            rest <- takeWhileP Nothing (\c -> c /= '{' && c /= '}' && c /= '<' && c /= '>')
            pure (buildTextNode (sp <> rest))

-- | Parses a hsx text node
--
-- Stops parsing when hitting a variable, like `{myVar}`
hsxText :: Parser Node
hsxText = buildTextNode <$> takeWhile1P (Just "text") (\c -> c /= '{' && c /= '}' && c /= '<' && c /= '>')

-- | Builds a TextNode and strips all surround whitespace from the input string
buildTextNode :: Text -> Node
buildTextNode value = TextNode (collapseSpace value)

hsxSplicedNode :: Parser Node
hsxSplicedNode = do
    (pos, expression) <- hsxSplicedExpression
    haskellExpression <- parseHaskellExpression pos (cs expression)
    pure (SplicedNode haskellExpression)

data SpliceScanMode
    = SpliceNormal
    | SpliceLineComment
    | SpliceBlockComment !Int
    | SpliceString
    | SpliceChar
    | SpliceQuasiQuote !Int
    deriving (Eq, Show)

-- | Consume the content of a @{ ... }@ splice, returning:
--   1) expression text (without surrounding braces)
--   2) amount of input consumed (including the closing @}@)
consumeSplicedExpression :: Text -> Either String (Text, Int)
consumeSplicedExpression input =
    case go 0 SpliceNormal [] 0 (Text.unpack input) of
        Left err -> Left err
        Right (chunks, consumed) -> Right (Text.concat (reverse chunks), consumed)
  where
    go :: Int -> SpliceScanMode -> [Text] -> Int -> String -> Either String ([Text], Int)
    go _ _ _ _ [] = Left "unterminated { } splice"
    go depth mode acc consumed s@(c:cs) =
        case mode of
            SpliceNormal
                | hasPrefix "--" s ->
                    go depth SpliceLineComment ("--" : acc) (consumed + 2) (Prelude.drop 2 s)
                | hasPrefix "{-" s ->
                    go depth (SpliceBlockComment 1) ("{-" : acc) (consumed + 2) (Prelude.drop 2 s)
                | c == '"' ->
                    go depth SpliceString ("\"" : acc) (consumed + 1) cs
                | c == '\'' ->
                    go depth SpliceChar ("'" : acc) (consumed + 1) cs
                | c == '[' && isQuasiQuoteStart cs ->
                    let (prefix, rest) = consumeQuasiQuotePrefix cs
                    in go depth (SpliceQuasiQuote 1) (Text.pack ('[' : prefix) : acc) (consumed + 1 + length prefix) rest
                | c == '{' ->
                    go (depth + 1) SpliceNormal ("{" : acc) (consumed + 1) cs
                | c == '}' && depth == 0 ->
                    Right (acc, consumed + 1)
                | c == '}' ->
                    go (depth - 1) SpliceNormal ("}" : acc) (consumed + 1) cs
                | otherwise ->
                    go depth SpliceNormal (Text.singleton c : acc) (consumed + 1) cs
            SpliceLineComment
                | c == '\n' ->
                    go depth SpliceNormal (Text.singleton c : acc) (consumed + 1) cs
                | otherwise ->
                    go depth SpliceLineComment (Text.singleton c : acc) (consumed + 1) cs
            SpliceBlockComment nesting
                | hasPrefix "{-" s ->
                    go depth (SpliceBlockComment (nesting + 1)) ("{-" : acc) (consumed + 2) (Prelude.drop 2 s)
                | hasPrefix "-}" s ->
                    if nesting == 1
                        then go depth SpliceNormal ("-}" : acc) (consumed + 2) (Prelude.drop 2 s)
                        else go depth (SpliceBlockComment (nesting - 1)) ("-}" : acc) (consumed + 2) (Prelude.drop 2 s)
                | otherwise ->
                    go depth (SpliceBlockComment nesting) (Text.singleton c : acc) (consumed + 1) cs
            SpliceString
                | c == '\\' ->
                    case cs of
                        [] -> Left "unterminated string literal in splice"
                        (d:ds) ->
                            go depth SpliceString (Text.pack ['\\', d] : acc) (consumed + 2) ds
                | c == '"' ->
                    go depth SpliceNormal ("\"" : acc) (consumed + 1) cs
                | otherwise ->
                    go depth SpliceString (Text.singleton c : acc) (consumed + 1) cs
            SpliceChar
                | c == '\\' ->
                    case cs of
                        [] -> Left "unterminated char literal in splice"
                        (d:ds) ->
                            go depth SpliceChar (Text.pack ['\\', d] : acc) (consumed + 2) ds
                | c == '\'' ->
                    go depth SpliceNormal ("'" : acc) (consumed + 1) cs
                | otherwise ->
                    go depth SpliceChar (Text.singleton c : acc) (consumed + 1) cs
            SpliceQuasiQuote qqDepth
                | c == '[' && isQuasiQuoteStart cs ->
                    let (prefix, rest) = consumeQuasiQuotePrefix cs
                    in go depth (SpliceQuasiQuote (qqDepth + 1)) (Text.pack ('[' : prefix) : acc) (consumed + 1 + length prefix) rest
                | hasPrefix "|]" s ->
                    if qqDepth == 1
                        then go depth SpliceNormal ("|]" : acc) (consumed + 2) (Prelude.drop 2 s)
                        else go depth (SpliceQuasiQuote (qqDepth - 1)) ("|]" : acc) (consumed + 2) (Prelude.drop 2 s)
                | otherwise ->
                    go depth (SpliceQuasiQuote qqDepth) (Text.singleton c : acc) (consumed + 1) cs

    hasPrefix :: String -> String -> Bool
    hasPrefix = List.isPrefixOf

    isQuasiQuoteStart :: String -> Bool
    isQuasiQuoteStart s =
        case parseQualifiedName s of
            Just (_, '|':_) -> True
            _ -> False

    consumeQuasiQuotePrefix :: String -> (String, String)
    consumeQuasiQuotePrefix s =
        case parseQualifiedName s of
            Just (name, '|':rest) -> (name ++ "|", rest)
            _ -> ("", s)

    parseQualifiedName :: String -> Maybe (String, String)
    parseQualifiedName s = do
        (first, rest) <- parseIdent s
        let (segments, rest') = parseMoreSegments rest
        pure (concat (first : segments), rest')
      where
        parseMoreSegments ('.':cs) =
            case parseIdent cs of
                Nothing -> ([], '.':cs)
                Just (seg, rest) ->
                    let (more, rest') = parseMoreSegments rest
                    in (("." ++ seg) : more, rest')
        parseMoreSegments other = ([], other)

    parseIdent :: String -> Maybe (String, String)
    parseIdent [] = Nothing
    parseIdent (x:xs)
        | Char.isAlpha x || x == '_' =
            let (tailName, rest) = span (\ch -> Char.isAlphaNum ch || ch == '_' || ch == '\'') xs
            in Just (x : tailName, rest)
        | otherwise = Nothing



hsxElementName :: Parser Text
hsxElementName = do
    name <- takeWhile1P (Just "identifier") (\c -> Char.isAlphaNum c || c == '_' || c == '-' || c == '!')
    -- Validate tag names case-insensitively against known parents and leafs.
    let nameCI = Text.toCaseFold name
    let isValidParent = nameCI `Set.member` parentsCI
    let isValidLeaf = nameCI `Set.member` leafsCI
    let isValidCustomWebComponent = "-" `Text.isInfixOf` name 
                                  && not (Text.isPrefixOf "-" name)
                                  && not (Char.isNumber (Text.head name))
    let isValidAdditionalTag =
            let additionalCI = Set.map Text.toCaseFold ?settings.additionalTagNames
            in nameCI `Set.member` additionalCI
    let checkingMarkup = ?settings.checkMarkup
    unless (isValidParent || isValidLeaf || isValidCustomWebComponent || isValidAdditionalTag || not checkingMarkup) do
        let allTags = parents `Set.union` leafs `Set.union` ?settings.additionalTagNames
        let suggestions = findSuggestions name allTags
        fail $ "Invalid tag name: " <> cs name <> cs (formatSuggestions suggestions)
    space
    pure name

hsxIdentifier :: Parser Text
hsxIdentifier = do
    name <- takeWhile1P (Just "identifier") (\c -> Char.isAlphaNum c || c == '_')
    space
    pure name


attributes :: Set Text
attributes = Set.fromList
        [ "accept", "accept-charset", "accesskey", "action", "alt", "async"
        , "autocapitalize", "autocomplete", "autofocus", "autoplay", "challenge", "charset"
        , "checked", "cite", "class", "cols", "colspan", "content"
        , "contenteditable", "contextmenu", "controls", "coords", "data"
        , "datetime", "defer", "dir", "disabled", "draggable", "enctype"
        , "form", "formaction", "formenctype", "formmethod", "formnovalidate"
        , "for"
        , "formtarget", "headers", "height", "hidden", "high", "href"
        , "hreflang", "http-equiv", "icon", "id", "ismap", "item", "itemprop"
        , "itemscope", "itemtype"
        , "keytype", "label", "lang", "list", "loop", "low", "manifest", "max"
        , "maxlength", "media", "method", "min", "multiple", "name"
        , "novalidate", "onbeforeonload", "onbeforeprint", "onblur", "oncanplay"
        , "oncanplaythrough", "onchange", "oncontextmenu", "onclick"
        , "ondblclick", "ondrag", "ondragend", "ondragenter", "ondragleave"
        , "ondragover", "ondragstart", "ondrop", "ondurationchange", "onemptied"
        , "onended", "onerror", "onfocus", "onformchange", "onforminput"
        , "onhaschange", "oninput", "oninvalid", "onkeydown", "onkeyup"
        , "onload", "onloadeddata", "onloadedmetadata", "onloadstart"
        , "onmessage", "onmousedown", "onmousemove", "onmouseout", "onmouseover"
        , "onmouseup", "onmousewheel", "ononline", "onpagehide", "onpageshow"
        , "onpause", "onplay", "onplaying", "onprogress", "onpropstate"
        , "onratechange", "onreadystatechange", "onredo", "onresize", "onscroll"
        , "onseeked", "onseeking", "onselect", "onstalled", "onstorage"
        , "onsubmit", "onsuspend", "ontimeupdate", "onundo", "onunload"
        , "onvolumechange", "onwaiting", "open", "optimum", "pattern", "ping"
        , "placeholder", "preload", "pubdate", "radiogroup", "readonly", "rel"
        , "required", "reversed", "rows", "rowspan", "sandbox", "scope"
        , "scoped", "seamless", "selected", "shape", "size", "sizes", "span"
        , "spellcheck", "src", "srcdoc", "srcset", "start", "step", "style", "subject"
        , "summary", "tabindex", "target", "title", "type", "usemap", "value"
        , "width", "wrap", "xmlns"
        , "ontouchstart", "download"
        , "allowtransparency", "minlength", "maxlength", "property"
        , "role"
        , "d", "viewBox", "cx", "cy", "r", "x", "y", "text-anchor", "alignment-baseline"
        , "line-spacing", "letter-spacing"
        , "integrity", "crossorigin", "poster"
        , "decoding", "fr", "mask-type", "paint-order", "side"
        , "text-overflow", "transform-origin", "vector-effect", "white-space"
        , "accent-height", "accumulate", "additive", "alphabetic", "amplitude"
        , "arabic-form", "ascent", "attributeName", "attributeType", "azimuth"
        , "baseFrequency", "baseProfile", "bbox", "begin", "bias", "by", "calcMode"
        , "cap-height", "class", "clipPathUnits", "contentScriptType"
        , "contentStyleType", "cx", "cy", "d", "descent", "diffuseConstant", "divisor"
        , "dur", "dx", "dy", "edgeMode", "elevation", "end", "exponent"
        , "externalResourcesRequired", "filterRes", "filterUnits", "font-family"
        , "font-size", "font-stretch", "font-style", "font-variant", "font-weight"
        , "format", "from", "fx", "fy", "g1", "g2", "glyph-name", "glyphRef"
        , "gradientTransform", "gradientUnits", "hanging", "height", "horiz-adv-x"
        , "horiz-origin-x", "horiz-origin-y", "id", "ideographic", "in", "in2"
        , "intercept", "k", "k1", "k2", "k3", "k4", "kernelMatrix", "kernelUnitLength"
        , "keyPoints", "keySplines", "keyTimes", "lang", "lengthAdjust"
        , "limitingConeAngle", "local", "markerHeight", "markerUnits", "markerWidth"
        , "maskContentUnits", "maskUnits", "mathematical", "max", "media", "method"
        , "min", "mode", "name", "numOctaves", "offset", "onabort", "onactivate"
        , "onbegin", "onclick", "onend", "onerror", "onfocusin", "onfocusout", "onload"
        , "onmousedown", "onmousemove", "onmouseout", "onmouseover", "onmouseup"
        , "onrepeat", "onresize", "onscroll", "onunload", "onzoom", "operator", "order"
        , "orient", "orientation", "origin", "overline-position", "overline-thickness"
        , "panose-1", "path", "pathLength", "patternContentUnits", "patternTransform"
        , "patternUnits", "points", "pointsAtX", "pointsAtY", "pointsAtZ"
        , "preserveAlpha", "preserveAspectRatio", "primitiveUnits", "r", "radius"
        , "refX", "refY", "rendering-intent", "repeatCount", "repeatDur"
        , "requiredExtensions", "requiredFeatures", "restart", "result", "rotate", "rx"
        , "ry", "scale", "seed", "slope", "spacing", "specularConstant"
        , "specularExponent", "spreadMethod", "startOffset", "stdDeviation", "stemh"
        , "stemv", "stitchTiles", "strikethrough-position", "strikethrough-thickness"
        , "string", "style", "surfaceScale", "systemLanguage", "tableValues", "target"
        , "targetX", "targetY", "textLength", "title", "to", "transform", "type", "u1"
        , "u2", "underline-position", "underline-thickness", "unicode", "unicode-range"
        , "units-per-em", "v-alphabetic", "v-hanging", "v-ideographic", "v-mathematical"
        , "values", "version", "vert-adv-y", "vert-origin-x", "vert-origin-y", "viewBox"
        , "viewTarget", "width", "widths", "x", "x-height", "x1", "x2"
        , "xChannelSelector", "xlink:actuate", "xlink:arcrole", "xlink:href"
        , "xlink:role", "xlink:show", "xlink:title", "xlink:type", "xml:base"
        , "xml:lang", "xml:space", "y", "y1", "y2", "yChannelSelector", "z", "zoomAndPan"
        , "alignment-baseline", "baseline-shift", "clip-path", "clip-rule"
        , "clip", "color-interpolation-filters", "color-interpolation"
        , "color-profile", "color-rendering", "color", "cursor", "direction"
        , "display", "dominant-baseline", "enable-background", "fill-opacity"
        , "fill-rule", "fill", "filter", "flood-color", "flood-opacity"
        , "font-size-adjust", "glyph-orientation-horizontal"
        , "glyph-orientation-vertical", "image-rendering", "kerning", "letter-spacing"
        , "lighting-color", "marker-end", "marker-mid", "marker-start", "mask"
        , "opacity", "overflow", "pointer-events", "shape-rendering", "stop-color"
        , "stop-opacity", "stroke-dasharray", "stroke-dashoffset", "stroke-linecap"
        , "stroke-linejoin", "stroke-miterlimit", "stroke-opacity", "stroke-width"
        , "stroke", "text-anchor", "text-decoration", "text-rendering", "unicode-bidi"
        , "visibility", "word-spacing", "writing-mode", "is"
        , "cellspacing", "cellpadding", "bgcolor", "classes"
        , "loading"
        , "frameborder", "allow", "allowfullscreen", "nonce", "referrerpolicy", "slot"
        , "kind"
        , "html"
        , "sse-connect", "sse-swap"
        , "muted", "onkeypress"
        ]

parents :: Set Text
parents = Set.fromList
          [ "a"
          , "abbr"
          , "address"
          , "animate"
          , "animateMotion"
          , "animateTransform"
          , "article"
          , "aside"
          , "audio"
          , "b"
          , "bdi"
          , "bdo"
          , "blink"
          , "blockquote"
          , "body"
          , "button"
          , "canvas"
          , "caption"
          , "circle"
          , "cite"
          , "clipPath"
          , "code"
          , "colgroup"
          , "command"
          , "data"
          , "datalist"
          , "dd"
          , "defs"
          , "del"
          , "desc"
          , "details"
          , "dfn"
          , "dialog"
          , "discard"
          , "div"
          , "dl"
          , "dt"
          , "ellipse"
          , "em"
          , "feBlend"
          , "feColorMatrix"
          , "feComponentTransfer"
          , "feComposite"
          , "feConvolveMatrix"
          , "feDiffuseLighting"
          , "feDisplacementMap"
          , "feDistantLight"
          , "feDropShadow"
          , "feFlood"
          , "feFuncA"
          , "feFuncB"
          , "feFuncG"
          , "feFuncR"
          , "feGaussianBlur"
          , "feImage"
          , "feMerge"
          , "feMergeNode"
          , "feMorphology"
          , "feOffset"
          , "fePointLight"
          , "feSpecularLighting"
          , "feSpotLight"
          , "feTile"
          , "feTurbulence"
          , "fieldset"
          , "figcaption"
          , "figure"
          , "filter"
          , "footer"
          , "foreignObject"
          , "form"
          , "g"
          , "h1"
          , "h2"
          , "h3"
          , "h4"
          , "h5"
          , "h6"
          , "hatch"
          , "hatchpath"
          , "head"
          , "header"
          , "hgroup"
          , "html"
          , "i"
          , "iframe"
          , "image"
          , "ins"
          , "ion-icon"
          , "kbd"
          , "label"
          , "legend"
          , "li"
          , "line"
          , "linearGradient"
          , "loading"
          , "main"
          , "map"
          , "mark"
          , "marker"
          , "marquee"
          , "mask"
          , "menu"
          , "mesh"
          , "meshgradient"
          , "meshpatch"
          , "meshrow"
          , "metadata"
          , "meter"
          , "mpath"
          , "nav"
          , "noscript"
          , "object"
          , "ol"
          , "optgroup"
          , "option"
          , "output"
          , "p"
          , "path"
          , "pattern"
          , "picture"
          , "polygon"
          , "polyline"
          , "pre"
          , "progress"
          , "q"
          , "radialGradient"
          , "rect"
          , "rp"
          , "rt"
          , "ruby"
          , "s"
          , "samp"
          , "script"
          , "section"
          , "select"
          , "set"
          , "slot"
          , "small"
          , "source"
          , "span"
          , "stop"
          , "strong"
          , "style"
          , "sub"
          , "summary"
          , "sup"
          , "svg"
          , "switch"
          , "symbol"
          , "table"
          , "tbody"
          , "td"
          , "template"
          , "text"
          , "textPath"
          , "textarea"
          , "tfoot"
          , "th"
          , "thead"
          , "time"
          , "title"
          , "tr"
          , "track"
          , "tspan"
          , "u"
          , "ul"
          , "unknown"
          , "use"
          , "var"
          , "video"
          , "view"
          , "wbr"
          ]

leafs :: Set Text
leafs = Set.fromList
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

-- Case-insensitive lookup sets for tags
parentsCI :: Set Text
parentsCI = Set.map Text.toCaseFold parents

leafsCI :: Set Text
leafsCI = Set.map Text.toCaseFold leafs

stripTextNodeWhitespaces :: [Node] -> [Node]
stripTextNodeWhitespaces nodes = stripLastTextNodeWhitespaces (stripFirstTextNodeWhitespaces nodes)

stripLastTextNodeWhitespaces :: [Node] -> [Node]
stripLastTextNodeWhitespaces [] = []
stripLastTextNodeWhitespaces [TextNode text] = [TextNode (Text.stripEnd text)]
stripLastTextNodeWhitespaces [node] = [node]
stripLastTextNodeWhitespaces (x:xs) = x : stripLastTextNodeWhitespaces xs

stripFirstTextNodeWhitespaces :: [Node] -> [Node]
stripFirstTextNodeWhitespaces [] = []
stripFirstTextNodeWhitespaces (TextNode text : rest) = TextNode (Text.stripStart text) : rest
stripFirstTextNodeWhitespaces nodes = nodes

-- | Computes the Damerau-Levenshtein edit distance between two texts.
-- Counts insertions, deletions, substitutions, and adjacent transpositions.
editDistance :: Text -> Text -> Int
editDistance a b = d Map.! (m, n)
    where
        m = Text.length a
        n = Text.length b
        d = Map.fromList [((i, j), cost i j) | i <- [0..m], j <- [0..n]]
        cost 0 j = j
        cost i 0 = i
        cost i j = minimum $
            [ (d Map.! (i-1, j)) + 1
            , (d Map.! (i, j-1)) + 1
            , (d Map.! (i-1, j-1)) + if Text.index a (i-1) == Text.index b (j-1) then 0 else 1
            ] ++
            [ (d Map.! (i-2, j-2)) + 1
            | i > 1, j > 1
            , Text.index a (i-1) == Text.index b (j-2)
            , Text.index a (i-2) == Text.index b (j-1)
            ]

-- | Find up to 3 closest suggestions from a set of valid names
findSuggestions :: Text -> Set Text -> [Text]
findSuggestions name validNames =
    let maxDist = max 2 (Text.length name `div` 2)
        candidates = [(n, editDistance name n) | n <- Set.toList validNames, n /= name]
        close = List.filter (\(_, d) -> d <= maxDist) candidates
    in List.map fst $ List.take 3 $ sortOn snd close

-- | Format a "Did you mean" suggestion string
formatSuggestions :: [Text] -> Text
formatSuggestions [] = ""
formatSuggestions [x] = "\n\nDid you mean: " <> x <> "?"
formatSuggestions xs = "\n\nDid you mean one of these: " <> Text.intercalate ", " xs <> "?"

-- | Hint when an attribute name looks like it's missing a prefix hyphen
missingPrefixHint :: Text -> Text
missingPrefixHint name
    | hasPrefix "data" && not (hasPrefix "data-") = hint "data-" 4
    | hasPrefix "aria" && not (hasPrefix "aria-") = hint "aria-" 4
    | hasPrefix "hx"   && not (hasPrefix "hx-")   = hint "hx-"   2
    | otherwise = ""
    where
        hasPrefix p = p `Text.isPrefixOf` name
        hint prefix dropLen = "\nDid you mean to use a " <> prefix <> " attribute (e.g. " <> prefix <> Text.drop dropLen name <> ")?"

-- | Replaces multiple space characters with a single one
collapseSpace :: Text -> Text
collapseSpace text
    | Text.null text = text
    | otherwise = Text.concat (go text)
  where
    go t
        | Text.null t = []
        | Char.isSpace (Text.head t) = " " : go (Text.dropWhile Char.isSpace t)
        | otherwise = let (w, rest) = Text.span (not . Char.isSpace) t in w : go rest

-- | Render a parsed HSX tree to static HTML.
-- Fails when the tree contains dynamic splices or attributes.
renderStaticNode :: Node -> Either Text Text
renderStaticNode (Node name _ _ _) | Text.toCaseFold name == "!doctype" = Right "<!DOCTYPE HTML>\n"
renderStaticNode (Node name attributes children isLeaf) = do
    renderedAttributes <- Text.concat <$> mapM renderStaticAttribute attributes
    renderedChildren <- Text.concat <$> mapM renderStaticNode children
    let openingTag = "<" <> name <> renderedAttributes <> ">"
    pure if isLeaf
        then openingTag
        else openingTag <> renderedChildren <> "</" <> name <> ">"
renderStaticNode (TextNode value) = Right value
renderStaticNode (PreEscapedTextNode value) = Right value
renderStaticNode (CommentNode value) = Right ("<!-- " <> value <> " -->")
renderStaticNode NoRenderCommentNode = Right ""
renderStaticNode (FragmentNode children) = Text.concat <$> mapM renderStaticNode children
renderStaticNode (Children children) = Text.concat <$> mapM renderStaticNode children
renderStaticNode (SplicedNode _) = Left "Dynamic splice nodes are not supported here"

renderStaticAttribute :: Attribute -> Either Text Text
renderStaticAttribute (StaticAttribute name (TextValue value)) =
    Right (" " <> name <> "=\"" <> value <> "\"")
renderStaticAttribute (StaticAttribute _ (ExpressionValue _)) =
    Left "Dynamic attribute expressions are not supported here"
renderStaticAttribute (SpreadAttributes _) =
    Left "Spread attributes are not supported here"
