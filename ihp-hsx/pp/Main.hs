module Main where

import Data.Char (isAlpha, isAlphaNum, isSpace, toLower)
import Data.List (elemIndex, isPrefixOf, isSuffixOf, foldl')
import System.Directory (canonicalizePath, getSymbolicLinkTarget, pathIsSymbolicLink)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            hPutStrLn stderr "ihp-hsx-pp: expected input file"
            exitFailure
        [input] -> do
            resolvedInput <- resolveInputPath input
            content <- readFile input
            let (shebang, rest) = splitShebang content
            let fileLevelHsx = shouldUseFileLevelHsx resolvedInput rest
            let needsTemplateHaskell = not (hasLanguagePragma "TemplateHaskell" rest)
            let needsQuasiQuotes = fileLevelHsx && not (hasLanguagePragma "QuasiQuotes" rest)
            let lineStart = if null shebang then 1 else 2
            let headerPragmas =
                    concat
                        [ if needsTemplateHaskell then ["{-# LANGUAGE TemplateHaskell #-}"] else []
                        , if needsQuasiQuotes then ["{-# LANGUAGE QuasiQuotes #-}"] else []
                        ]
            let header =
                    shebang
                    ++ (if null headerPragmas then "" else unlines headerPragmas ++ "{-# LINE " ++ show lineStart ++ " " ++ show input ++ " #-}\n")
            let rewritten = ensureQuoteExpImport (rewriteContent fileLevelHsx rest)
            putStr (header ++ rewritten)
        _ -> do
            let input = args !! (length args - 2)
            let output = args !! (length args - 1)
            resolvedInput <- resolveInputPath input
            content <- readFile input
            let (shebang, rest) = splitShebang content
            let fileLevelHsx = shouldUseFileLevelHsx resolvedInput rest
            let needsTemplateHaskell = not (hasLanguagePragma "TemplateHaskell" rest)
            let needsQuasiQuotes = fileLevelHsx && not (hasLanguagePragma "QuasiQuotes" rest)
            let lineStart = if null shebang then 1 else 2
            let headerPragmas =
                    concat
                        [ if needsTemplateHaskell then ["{-# LANGUAGE TemplateHaskell #-}"] else []
                        , if needsQuasiQuotes then ["{-# LANGUAGE QuasiQuotes #-}"] else []
                        ]
            let header =
                    shebang
                    ++ (if null headerPragmas then "" else unlines headerPragmas ++ "{-# LINE " ++ show lineStart ++ " " ++ show input ++ " #-}\n")
            let rewritten = ensureQuoteExpImport (rewriteContent fileLevelHsx rest)
            writeFile output (header ++ rewritten)

splitShebang :: String -> (String, String)
splitShebang input =
    case lines input of
        (first:rest) | "#!" `isPrefixOf` first -> (first ++ "\n", unlines rest)
        _ -> ("", input)

resolveInputPath :: FilePath -> IO FilePath
resolveInputPath input = do
    isLink <- pathIsSymbolicLink input
    if isLink
        then do
            target <- getSymbolicLinkTarget input
            canonicalizePath (takeDirectory input </> target)
        else canonicalizePath input

hasLanguagePragma :: String -> String -> Bool
hasLanguagePragma extension input = any hasLine (lines input)
  where
    hasLine line =
        "{-#" `isPrefixOf` dropWhile (== ' ') line
        && ("LANGUAGE" `isInfixOf` line || "OPTIONS_GHC" `isInfixOf` line)
        && extension `isInfixOf` line

hasFileLevelHsxMarker :: String -> Bool
hasFileLevelHsxMarker input = scanHeader 0 (lines input)
  where
    -- Header scope: language/options pragmas, blank lines, and comments that
    -- appear before the module declaration or the first non-header code line.
    --
    -- Markers are recognized only in top-level line comments (`-- hsx`), not
    -- inside block comments.
    scanHeader _ [] = False
    scanHeader depth (line:rest)
        | depth > 0 = scanHeader (updateBlockDepth depth line) rest
        | isPragmaLine line = scanHeader depth rest
        | all isSpace line = scanHeader depth rest
        | startsBlockComment line = scanHeader (updateBlockDepth depth line) rest
        | isLineComment line = isMarkerLine line || scanHeader depth rest
        | isModuleLine line = False
        | otherwise = False

    isMarkerLine line =
        case dropWhile isSpace line of
            '-':'-':rest ->
                let token = map toLower (trim (dropWhile isSpace rest))
                in token == "hsx" || token == "hsx-file"
            _ -> False
    trim = reverse . dropWhile isSpace . reverse
    isLineComment line =
        case dropWhile isSpace line of
            '-':'-':_ -> True
            _ -> False
    startsBlockComment line =
        "{-" `isPrefixOf` dropWhile isSpace line
    updateBlockDepth depth line = goDepth depth line
      where
        goDepth d [] = d
        goDepth d s
            | isPrefixOf "{-" s = goDepth (d + 1) (drop 2 s)
            | isPrefixOf "-}" s = goDepth (max 0 (d - 1)) (drop 2 s)
            | otherwise = goDepth d (drop 1 s)

shouldUseFileLevelHsx :: FilePath -> String -> Bool
shouldUseFileLevelHsx inputPath inputContent =
    hasFileLevelHsxMarker inputContent || isHsxFile inputPath

isHsxFile :: FilePath -> Bool
isHsxFile path =
    ".hsx" `isSuffixOf` map toLower path

rewriteContent :: Bool -> String -> String
rewriteContent fileLevelHsx = go Normal ""
  where
    go _ _ [] = []
    go Normal lineBuf s@(c:cs)
        | c == '-' && isPrefixOf "--" s = "--" ++ go LineComment (lineBuf ++ "--") (drop 2 s)
        | c == '{' && isPrefixOf "{-" s = "{-" ++ go (BlockComment 1) (lineBuf ++ "{-") (drop 2 s)
        | c == '"' = '"' : go StringLit (lineBuf ++ "\"") cs
        | c == '\'' = '\'' : go CharLit (lineBuf ++ "'") cs
        | fileLevelHsx && c == '<' && looksLikeTagStart cs && shouldStartHsxInLine lineBuf =
            let (body, rest) = consumeHsxBlock s
                replacement = "$(quoteExp hsx " ++ show body ++ ")"
            in replacement ++ go Normal (lineBuf ++ "hsx") rest
        | c == '[' =
            case parseQQStart cs of
                Just (qqName, restAfterBar) | isTargetQQ qqName ->
                    let (body, rest) = consumeQQ 1 restAfterBar
                        replacement = "$(quoteExp " ++ qqName ++ " " ++ show body ++ ")"
                    in replacement ++ go Normal (lineBuf ++ "hsx") rest
                Just (qqName, restAfterBar) ->
                    let (body, rest) = consumeQQRaw restAfterBar
                        raw = "[" ++ qqName ++ "|" ++ body ++ "|]"
                        lineBuf' = updateLineBuf lineBuf raw
                    in raw ++ go Normal lineBuf' rest
                _ -> '[' : go Normal (lineBuf ++ "[") cs
        | c == '\n' = '\n' : go Normal "" cs
        | otherwise = c : go Normal (lineBuf ++ [c]) cs
    go LineComment lineBuf s@(c:cs)
        | c == '\n' = '\n' : go Normal "" cs
        | otherwise = c : go LineComment (lineBuf ++ [c]) cs
    go (BlockComment depth) lineBuf s
        | isPrefixOf "{-" s = "{-" ++ go (BlockComment (depth + 1)) (lineBuf ++ "{-") (drop 2 s)
        | isPrefixOf "-}" s =
            let depth' = depth - 1
            in "-}" ++ if depth' == 0 then go Normal (lineBuf ++ "-}") (drop 2 s) else go (BlockComment depth') (lineBuf ++ "-}") (drop 2 s)
        | otherwise =
            case s of
                (c:cs) ->
                    if c == '\n'
                        then c : go (BlockComment depth) "" cs
                        else c : go (BlockComment depth) (lineBuf ++ [c]) cs
    go StringLit lineBuf s@(c:cs)
        | c == '\\' = case cs of
            [] -> "\\"
            (d:ds) -> '\\' : d : go StringLit (lineBuf ++ ['\\', d]) ds
        | c == '"' = '"' : go Normal (lineBuf ++ "\"") cs
        | c == '\n' = '\n' : go StringLit "" cs
        | otherwise = c : go StringLit (lineBuf ++ [c]) cs
    go CharLit lineBuf s@(c:cs)
        | c == '\\' = case cs of
            [] -> "\\"
            (d:ds) -> '\\' : d : go CharLit (lineBuf ++ ['\\', d]) ds
        | c == '\'' = '\'' : go Normal (lineBuf ++ "'") cs
        | c == '\n' = '\n' : go CharLit "" cs
        | otherwise = c : go CharLit (lineBuf ++ [c]) cs

data Mode = Normal | LineComment | BlockComment Int | StringLit | CharLit

advanceLineStart :: Bool -> Char -> Bool
advanceLineStart lineStart c
    | c == '\n' = True
    | lineStart && (c == ' ' || c == '\t') = True
    | otherwise = False

consumeQQ :: Int -> String -> (String, String)
consumeQQ depth = go depth []
  where
    go _ _ [] = error "ihp-hsx-pp: unterminated hsx quasiquote"
    go d acc s
        | isPrefixOf "|]" s =
            if d == 1
                then (reverse acc, drop 2 s)
                else go (d - 1) (pushString "|]" acc) (drop 2 s)
        | isPrefixOf "[" s =
            case parseQQStart (drop 1 s) of
                Just (qqName, restAfterBar) | isTargetQQ qqName ->
                    let acc' = pushString ("[" ++ qqName ++ "|") acc
                    in go (d + 1) acc' restAfterBar
                _ -> case s of
                    (c:cs) -> go d (c:acc) cs
        | otherwise =
            case s of
                (c:cs) -> go d (c:acc) cs

consumeQQRaw :: String -> (String, String)
consumeQQRaw = go []
  where
    go _ [] = error "ihp-hsx-pp: unterminated quasiquote"
    go acc s
        | isPrefixOf "|]" s = (reverse acc, drop 2 s)
        | otherwise =
            case s of
                (c:cs) -> go (c:acc) cs

pushString :: String -> [Char] -> [Char]
pushString str acc = foldl' (flip (:)) acc str

breakOn :: String -> String -> (String, String)
breakOn needle = go []
  where
    needleLen = length needle
    go acc [] = (reverse acc, [])
    go acc s
        | needle `isPrefixOf` s = (reverse acc, drop needleLen s)
        | otherwise =
            case s of
                (c:cs) -> go (c:acc) cs

parseQQStart :: String -> Maybe (String, String)
parseQQStart s = do
    (name, rest) <- parseQualifiedName s
    case rest of
        ('|':more) -> Just (name, more)
        _ -> Nothing

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
parseIdent (c:cs)
    | isIdentStart c =
        let (nameTail, rest) = span isIdentChar cs
        in Just (c:nameTail, rest)
    | otherwise = Nothing

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

isTargetQQ :: String -> Bool
isTargetQQ name = baseName name `elem` targetNames

baseName :: String -> String
baseName = reverse . takeWhile (/= '.') . reverse

targetNames :: [String]
targetNames = ["hsx", "uncheckedHsx", "customHsx"]

looksLikeTagStart :: String -> Bool
looksLikeTagStart [] = False
looksLikeTagStart (c:_)
    | c == '/' = False
    | c == '>' = True
    | c == '!' = True
    | c == '?' = True
    | isTagNameStart c = True
    | otherwise = False

isTagNameStart :: Char -> Bool
isTagNameStart c = isAlpha c

isTagNameChar :: Char -> Bool
isTagNameChar c = isAlphaNum c || c == '-' || c == '_' || c == ':'

data TagInfo
    = TagOpen String
    | TagClose String
    | TagSelf String
    | TagSpecial
    deriving (Eq, Show)

consumeHsxBlock :: String -> (String, String)
consumeHsxBlock s =
    let (tagText, info, rest) = consumeTagText s
        (stack, rawTag) = updateStack [] Nothing info
    in if null stack
        then (tagText, rest)
        else
            let (body, rest') = consumeHsxBody stack rawTag rest
            in (tagText ++ body, rest')

consumeHsxBody :: [String] -> Maybe String -> String -> (String, String)
consumeHsxBody _ _ [] = error "ihp-hsx-pp: unterminated hsx block"
consumeHsxBody stack rawTag s
    | Just tagName <- rawTag =
        let (rawText, rest) = scanUntilClosingTag tagName s
        in case rest of
            [] -> error "ihp-hsx-pp: unterminated raw-text hsx block"
            _ ->
                let (tagText, info, rest') = consumeTagText rest
                    (stack', rawTag') = updateStack stack rawTag info
                in if null stack'
                    then (rawText ++ tagText, rest')
                    else
                        let (more, rest'') = consumeHsxBody stack' rawTag' rest'
                        in (rawText ++ tagText ++ more, rest'')
    | otherwise =
        case s of
            ('{':rest) ->
                let (content, rest') = consumeBraceRaw rest
                    rewritten = rewriteHaskellSegment content
                    (more, rest'') = consumeHsxBody stack rawTag rest'
                in ("{" ++ rewritten ++ "}" ++ more, rest'')
            ('<':_) ->
                let (tagText, info, rest') = consumeTagText s
                    (stack', rawTag') = updateStack stack rawTag info
                in if null stack'
                    then (tagText, rest')
                    else
                        let (more, rest'') = consumeHsxBody stack' rawTag' rest'
                        in (tagText ++ more, rest'')
            (c:cs) ->
                let (more, rest') = consumeHsxBody stack rawTag cs
                in (c:more, rest')

updateStack :: [String] -> Maybe String -> TagInfo -> ([String], Maybe String)
updateStack stack rawTag info =
    case info of
        TagOpen name ->
            let stack' = name : stack
                rawTag' = if isRawTag name then Just name else rawTag
            in (stack', rawTag')
        TagClose name ->
            let stack' = popMatching name stack
                rawTag' = case rawTag of
                    Just rawName | normalizeName rawName == normalizeName name -> Nothing
                    _ -> rawTag
            in (stack', rawTag')
        TagSelf _ -> (stack, rawTag)
        TagSpecial -> (stack, rawTag)

isRawTag :: String -> Bool
isRawTag name =
    case normalizeName name of
        "script" -> True
        "style" -> True
        _ -> False

normalizeName :: String -> String
normalizeName = map toLower

popMatching :: String -> [String] -> [String]
popMatching _ [] = []
popMatching name (x:xs)
    | normalizeName name == normalizeName x = xs
    | otherwise = popMatching name xs

consumeTagText :: String -> (String, TagInfo, String)
consumeTagText s@('<':'!':'-':'-':rest) =
    let (body, rest') = breakOn "-->" rest
    in ("<!--" ++ body ++ "-->", TagSpecial, rest')
consumeTagText s@('<':rest) =
    let (body, rest') = consumeTagBody rest
        tagText = "<" ++ body
        info = classifyTag tagText
    in (tagText, info, rest')
consumeTagText _ = error "ihp-hsx-pp: expected tag start"

consumeTagBody :: String -> (String, String)
consumeTagBody = go Nothing []
  where
    go _ _ [] = error "ihp-hsx-pp: unterminated tag"
    go quote acc s@(c:cs)
        | quote == Nothing && c == '"' = go (Just '"') (c:acc) cs
        | quote == Just '"' && c == '"' = go Nothing (c:acc) cs
        | quote == Nothing && c == '\'' = go (Just '\'') (c:acc) cs
        | quote == Just '\'' && c == '\'' = go Nothing (c:acc) cs
        | quote == Nothing && c == '{' =
            let (content, rest) = consumeBraceRaw cs
                rewritten = rewriteHaskellSegment content
                acc' = pushString ("{" ++ rewritten ++ "}") acc
            in go Nothing acc' rest
        | quote == Nothing && c == '>' = (reverse ('>':acc), cs)
        | otherwise = go quote (c:acc) cs

classifyTag :: String -> TagInfo
classifyTag tagText
    | tagText == "<>" = TagOpen fragmentTagName
    | tagText == "</>" = TagClose fragmentTagName
    | isPrefixOf "<!--" tagText = TagSpecial
    | isPrefixOf "<?" tagText = TagSpecial
    | isPrefixOf "<!" tagText = TagSpecial
    | isPrefixOf "</" tagText =
        case parseTagNameFrom (drop 2 tagText) of
            Just name -> TagClose name
            Nothing -> TagSpecial
    | otherwise =
        case parseTagNameFrom (drop 1 tagText) of
            Just name ->
                if isSelfClosing tagText || isVoidTag name
                    then TagSelf name
                    else TagOpen name
            Nothing -> TagSpecial

fragmentTagName :: String
fragmentTagName = "#hsx-fragment#"

parseTagNameFrom :: String -> Maybe String
parseTagNameFrom s =
    let rest = dropWhile isSpace s
        (name, _) = span isTagNameChar rest
    in if null name then Nothing else Just name

isSelfClosing :: String -> Bool
isSelfClosing tagText =
    case dropWhile isSpace (reverse tagText) of
        ('>':'/':_) -> True
        _ -> False

isVoidTag :: String -> Bool
isVoidTag name =
    normalizeName name `elem`
        [ "area", "base", "br", "col", "embed", "hr", "img"
        , "input", "keygen", "link", "meta", "param", "source"
        , "track", "wbr"
        ]

scanUntilClosingTag :: String -> String -> (String, String)
scanUntilClosingTag tagName = go []
  where
    go acc [] = (reverse acc, [])
    go acc s@(c:cs)
        | isClosingTagStart tagName s = (reverse acc, s)
        | otherwise = go (c:acc) cs

isClosingTagStart :: String -> String -> Bool
isClosingTagStart tagName s =
    case s of
        '<':'/':rest ->
            let rest' = dropWhile isSpace rest
                (name, tail') = span isTagNameChar rest'
                matches = normalizeName name == normalizeName tagName
            in matches && case dropWhile isSpace tail' of
                ('>':_) -> True
                _ -> False
        _ -> False

rewriteHaskellSegment :: String -> String
rewriteHaskellSegment = go Normal ""
  where
    go _ _ [] = []
    go Normal lineBuf s@(c:cs)
        | c == '-' && isPrefixOf "--" s = "--" ++ go LineComment (lineBuf ++ "--") (drop 2 s)
        | c == '{' && isPrefixOf "{-" s = "{-" ++ go (BlockComment 1) (lineBuf ++ "{-") (drop 2 s)
        | c == '"' = '"' : go StringLit (lineBuf ++ "\"") cs
        | c == '\'' = '\'' : go CharLit (lineBuf ++ "'") cs
        | c == '<' && looksLikeTagStart cs && shouldStartHsxInLine lineBuf =
            let (body, rest) = consumeHsxBlock s
                replacement = "$(quoteExp hsx " ++ show body ++ ")"
            in replacement ++ go Normal (lineBuf ++ "hsx") rest
        | c == '[' =
            case parseQQStart cs of
                Just (qqName, restAfterBar) ->
                    let (body, rest) = consumeQQRaw restAfterBar
                        raw = "[" ++ qqName ++ "|" ++ body ++ "|]"
                        lineBuf' = updateLineBuf lineBuf raw
                    in raw ++ go Normal lineBuf' rest
                _ -> '[' : go Normal (lineBuf ++ "[") cs
        | c == '\n' = '\n' : go Normal "" cs
        | otherwise = c : go Normal (lineBuf ++ [c]) cs
    go LineComment lineBuf s@(c:cs)
        | c == '\n' = '\n' : go Normal "" cs
        | otherwise = c : go LineComment (lineBuf ++ [c]) cs
    go (BlockComment depth) lineBuf s
        | isPrefixOf "{-" s = "{-" ++ go (BlockComment (depth + 1)) (lineBuf ++ "{-") (drop 2 s)
        | isPrefixOf "-}" s =
            let depth' = depth - 1
            in "-}" ++ if depth' == 0 then go Normal (lineBuf ++ "-}") (drop 2 s) else go (BlockComment depth') (lineBuf ++ "-}") (drop 2 s)
        | otherwise =
            case s of
                (c:cs) ->
                    if c == '\n'
                        then c : go (BlockComment depth) "" cs
                        else c : go (BlockComment depth) (lineBuf ++ [c]) cs
    go StringLit lineBuf s@(c:cs)
        | c == '\\' = case cs of
            [] -> "\\"
            (d:ds) -> '\\' : d : go StringLit (lineBuf ++ ['\\', d]) ds
        | c == '"' = '"' : go Normal (lineBuf ++ "\"") cs
        | c == '\n' = '\n' : go StringLit "" cs
        | otherwise = c : go StringLit (lineBuf ++ [c]) cs
    go CharLit lineBuf s@(c:cs)
        | c == '\\' = case cs of
            [] -> "\\"
            (d:ds) -> '\\' : d : go CharLit (lineBuf ++ ['\\', d]) ds
        | c == '\'' = '\'' : go Normal (lineBuf ++ "'") cs
        | c == '\n' = '\n' : go CharLit "" cs
        | otherwise = c : go CharLit (lineBuf ++ [c]) cs

shouldStartHsxInLine :: String -> Bool
shouldStartHsxInLine lineBuf =
    let trimmed = rstrip lineBuf
    in null trimmed
        || endsWithSymbol trimmed '='
        || endsWithSymbol trimmed '('
        || endsWithSymbol trimmed '['
        || endsWithSymbol trimmed '{'
        || endsWithSymbol trimmed ','
        || endsWithSymbol trimmed ';'
        || endsWithOperator trimmed "->"
        || endsWithWord trimmed "then"
        || endsWithWord trimmed "else"
        || endsWithWord trimmed "of"
        || endsWithWord trimmed "in"

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

endsWithSymbol :: String -> Char -> Bool
endsWithSymbol s c =
    case reverse s of
        (x:_) -> x == c
        _ -> False

endsWithOperator :: String -> String -> Bool
endsWithOperator s op =
    op `isSuffixOf` s

endsWithWord :: String -> String -> Bool
endsWithWord s word =
    let len = length word
    in word `isSuffixOf` s
        && (length s == len || not (isAlpha (s !! (length s - len - 1))))

updateLineBuf :: String -> String -> String
updateLineBuf lineBuf raw =
    case elemIndex '\n' (reverse raw) of
        Nothing -> lineBuf ++ raw
        Just idxFromEnd ->
            let idx = length raw - idxFromEnd - 1
            in drop (idx + 1) raw

consumeBraceRaw :: String -> (String, String)
consumeBraceRaw = go 1 Normal []
  where
    go _ _ acc [] = error "ihp-hsx-pp: unterminated { } splice"
    go depth mode acc s
        | depth == 0 = (reverse acc, s)
        | otherwise =
            case mode of
                Normal ->
                    if isPrefixOf "--" s
                        then go depth LineComment (pushString "--" acc) (drop 2 s)
                        else if isPrefixOf "{-" s
                            then go depth (BlockComment 1) (pushString "{-" acc) (drop 2 s)
                            else case s of
                                ('"':cs) -> go depth StringLit ('"':acc) cs
                                ('\'':cs) -> go depth CharLit ('\'':acc) cs
                                ('{':cs) -> go (depth + 1) Normal ('{':acc) cs
                                ('}':cs) ->
                                    if depth == 1
                                        then (reverse acc, cs)
                                        else go (depth - 1) Normal ('}':acc) cs
                                (c:cs) -> go depth Normal (c:acc) cs
                LineComment ->
                    case s of
                        (c:cs) ->
                            if c == '\n'
                                then go depth Normal (c:acc) cs
                                else go depth LineComment (c:acc) cs
                BlockComment commentDepth ->
                    if isPrefixOf "{-" s
                        then go depth (BlockComment (commentDepth + 1)) (pushString "{-" acc) (drop 2 s)
                        else if isPrefixOf "-}" s
                            then
                                let commentDepth' = commentDepth - 1
                                in if commentDepth' == 0
                                    then go depth Normal (pushString "-}" acc) (drop 2 s)
                                    else go depth (BlockComment commentDepth') (pushString "-}" acc) (drop 2 s)
                            else case s of
                                (c:cs) -> go depth (BlockComment commentDepth) (c:acc) cs
                StringLit ->
                    case s of
                        ('\\':cs) ->
                            case cs of
                                [] -> go depth StringLit ('\\':acc) []
                                (d:ds) -> go depth StringLit (d:'\\':acc) ds
                        ('"':cs) -> go depth Normal ('"':acc) cs
                        (c:cs) -> go depth StringLit (c:acc) cs
                CharLit ->
                    case s of
                        ('\\':cs) ->
                            case cs of
                                [] -> go depth CharLit ('\\':acc) []
                                (d:ds) -> go depth CharLit (d:'\\':acc) ds
                        ('\'':cs) -> go depth Normal ('\'':acc) cs
                        (c:cs) -> go depth CharLit (c:acc) cs

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:rest) = xs : tails rest

ensureQuoteExpImport :: String -> String
ensureQuoteExpImport input
    | "Language.Haskell.TH.Quote" `isInfixOf` input = input
    | otherwise =
        let ls = lines input
            importLine = "import Language.Haskell.TH.Quote (quoteExp)"
            insertAfter idx = unlines (take (idx + 1) ls ++ [importLine] ++ drop (idx + 1) ls)
        in case findModuleHeaderEnd ls of
            Just idx -> insertAfter idx
            Nothing -> unlines (insertAfterPragmas ls importLine)

findModuleHeaderEnd :: [String] -> Maybe Int
findModuleHeaderEnd ls = do
    start <- findModuleLine ls
    findWhereLine start ls

findModuleLine :: [String] -> Maybe Int
findModuleLine = go 0
  where
    go _ [] = Nothing
    go i (l:ls)
        | isModuleLine l = Just i
        | otherwise = go (i + 1) ls

findWhereLine :: Int -> [String] -> Maybe Int
findWhereLine start ls = go start (drop start ls)
  where
    go _ [] = Nothing
    go i (l:rest)
        | lineContainsWhere l = Just i
        | otherwise = go (i + 1) rest

isModuleLine :: String -> Bool
isModuleLine line =
    case dropWhile isSpace line of
        ('m':'o':'d':'u':'l':'e':_) -> True
        _ -> False

lineContainsWhere :: String -> Bool
lineContainsWhere line = "where" `isInfixOf` line

insertAfterPragmas :: [String] -> String -> [String]
insertAfterPragmas ls importLine =
    let (pragmas, rest) = span isPragmaLine ls
    in pragmas ++ [importLine] ++ rest

isPragmaLine :: String -> Bool
isPragmaLine line =
    case dropWhile isSpace line of
        ('{':'-':'#':_) -> True
        _ -> False
