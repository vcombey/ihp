{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import Data.Char (isAlpha, isAlphaNum, isSpace, toLower)
import Data.Data (Data, cast, gmapQ)
import Data.List (elemIndex, isPrefixOf, isSuffixOf, foldl', sortOn)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Directory (canonicalizePath, getSymbolicLinkTarget, pathIsSymbolicLink)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsExpr (..), HsModule, LocatedA (..))
import qualified GHC.Parser as GHCParser
import qualified GHC.Parser.Lexer as GHCLexer
import GHC.Parser.Lexer (ParseResult (..), PState (..))
import GHC.Types.Error
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (mkRealSrcLoc)
import GHC.Utils.Error
import GHC.Utils.Outputable hiding ((<>))
import GHC
#if __GLASGOW_HASKELL__ >= 908
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
#endif

data PreprocessorOptions = PreprocessorOptions
    { defaultFileLevelQQ :: String
    }

defaultPreprocessorOptions :: PreprocessorOptions
defaultPreprocessorOptions =
    PreprocessorOptions
        { defaultFileLevelQQ = "hsx"
        }

main :: IO ()
main = do
    args <- getArgs
    case parseInvocationArgs args of
        Left err -> do
            hPutStrLn stderr ("ihp-hsx-pp: " <> err)
            exitFailure
        Right (optionArgs, input, outputPath) -> do
            options <- case parsePreprocessorOptions optionArgs of
                Left err -> do
                    hPutStrLn stderr ("ihp-hsx-pp: " <> err)
                    exitFailure
                Right parsed -> pure parsed
            rewritten <- preprocessInput options input
            case outputPath of
                Nothing -> putStr rewritten
                Just output -> writeFile output rewritten

parseInvocationArgs :: [String] -> Either String ([String], FilePath, Maybe FilePath)
parseInvocationArgs [] = Left "expected input file"
parseInvocationArgs [input] = Right ([], input, Nothing)
parseInvocationArgs args =
    let secondToLast = args !! (length args - 2)
    in if isOptionArg secondToLast
        then Right (take (length args - 1) args, last args, Nothing)
        else Right (take (length args - 2) args, secondToLast, Just (last args))
  where
    isOptionArg ('-':_) = True
    isOptionArg _ = False

parsePreprocessorOptions :: [String] -> Either String PreprocessorOptions
parsePreprocessorOptions args = do
    customQQ <- foldl' step (Right Nothing) args
    let fileLevelQQ = fromMaybe (defaultFileLevelQQ defaultPreprocessorOptions) customQQ
    pure PreprocessorOptions
        { defaultFileLevelQQ = fileLevelQQ
        }
  where
    step :: Either String (Maybe String) -> String -> Either String (Maybe String)
    step state option = do
        customQQ <- state
        case () of
            _ | Just qq <- stripPrefixOption "--hsx-qq=" option ->
                    if null qq
                        then Left "--hsx-qq expects a non-empty quasiquoter name"
                        else Right (Just qq)
              | "--hsx-" `isPrefixOf` option ->
                    Left ("unknown option: " <> option)
              | otherwise ->
                    Right customQQ

    stripPrefixOption prefix option =
        if prefix `isPrefixOf` option
            then Just (drop (length prefix) option)
            else Nothing

preprocessInput :: PreprocessorOptions -> FilePath -> IO String
preprocessInput options input = do
    resolvedInput <- resolveInputPath input
    content <- readFile input
    pure (renderProcessedInput options input resolvedInput content)

renderProcessedInput :: PreprocessorOptions -> FilePath -> FilePath -> String -> String
renderProcessedInput options input resolvedInput content =
    let
        (shebang, rest) = splitShebang content
        fileLevelQQ = resolveFileLevelQQ options resolvedInput rest
        rewrittenBody = rewriteContent fileLevelQQ rest
        needsQuoteExp = "$(quoteExp " `isInfixOf` rewrittenBody
        needsTemplateHaskell = needsQuoteExp && not (hasLanguagePragma "TemplateHaskell" rest)
        needsQuasiQuotes = isJust fileLevelQQ && not (hasLanguagePragma "QuasiQuotes" rest)
        lineStart = if null shebang then 1 else 2
        headerPragmas =
            concat
                [ if needsTemplateHaskell then ["{-# LANGUAGE TemplateHaskell #-}"] else []
                , if needsQuasiQuotes then ["{-# LANGUAGE QuasiQuotes #-}"] else []
                ]
        header =
            shebang
            ++ (if null headerPragmas then "" else unlines headerPragmas ++ "{-# LINE " ++ show lineStart ++ " " ++ show input ++ " #-}\n")
        rewritten =
            if needsQuoteExp
                then ensureQuoteExpImport rewrittenBody
                else rewrittenBody
    in
        header ++ rewritten

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

findFileLevelHsxMarker :: String -> Maybe (Maybe String)
findFileLevelHsxMarker input = scanHeader 0 (lines input)
  where
    -- Header scope: language/options pragmas, blank lines, and comments that
    -- appear before the module declaration or the first non-header code line.
    --
    -- Markers are recognized only in top-level line comments (`-- hsx`), not
    -- inside block comments.
    scanHeader _ [] = Nothing
    scanHeader depth (line:rest)
        | depth > 0 = scanHeader (updateBlockDepth depth line) rest
        | isPragmaLine line = scanHeader depth rest
        | all isSpace line = scanHeader depth rest
        | startsBlockComment line = scanHeader (updateBlockDepth depth line) rest
        | isLineComment line =
            case parseMarkerLine line of
                Just marker -> Just marker
                Nothing -> scanHeader depth rest
        | isModuleLine line = Nothing
        | otherwise = Nothing

    parseMarkerLine line =
        case dropWhile isSpace line of
            '-':'-':rest ->
                case words (trim (dropWhile isSpace rest)) of
                    [keyword]
                        | isFileLevelKeyword keyword -> Just Nothing
                    [keyword, qqName]
                        | isFileLevelKeyword keyword -> Just (Just qqName)
                    _ -> Nothing
            _ -> Nothing

    isFileLevelKeyword keyword =
        let lowered = map toLower keyword
        in lowered == "hsx" || lowered == "hsx-file"

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

resolveFileLevelQQ :: PreprocessorOptions -> FilePath -> String -> Maybe String
resolveFileLevelQQ options inputPath inputContent =
    case findFileLevelHsxMarker inputContent of
        Just markerQQ -> Just (fromMaybe (defaultFileLevelQQ options) markerQQ)
        Nothing
            | isHsxFile inputPath -> Just (defaultFileLevelQQ options)
            | otherwise -> Nothing

isHsxFile :: FilePath -> Bool
isHsxFile path =
    ".hsx" `isSuffixOf` map toLower path

rewriteContent :: Maybe String -> String -> String
rewriteContent Nothing input = input
rewriteContent (Just fileLevelQQ) input =
    case rewriteContentRigorous fileLevelQQ input of
        Right rewritten -> rewritten
        Left _ -> rewriteContentHeuristic (Just fileLevelQQ) input

rewriteContentRigorous :: String -> String -> Either String String
rewriteContentRigorous fileLevelQQ input = do
    let detectedCandidates = detectHsxCandidates fileLevelQQ input
    if null detectedCandidates
        then
            if containsLikelyTagStart input
                then case parseModuleWithGhc "<ihp-hsx-pp>" input of
                    Right _ -> Right input
                    Left _ -> Left "fallback to heuristic rewrite"
                else Right input
        else do
            let placeholderPrefix = pickPlaceholderPrefix input
            let candidates = assignPlaceholderNames placeholderPrefix detectedCandidates
            let allPlaceholderInput = applyCandidateReplacement input candidates candidatePlaceholderName
            moduleAst <- parseModuleWithGhc "<ihp-hsx-pp>" allPlaceholderInput
            let expressionPlaceholders = collectExpressionPlaceholders placeholderPrefix moduleAst
            let expressionCandidates = Prelude.filter (\candidate -> candidatePlaceholderName candidate `Set.member` expressionPlaceholders) candidates
            let requiredCandidates = Prelude.filter (isRequiredHsxCandidate input candidates) expressionCandidates
            pure (applyCandidateReplacement input requiredCandidates candidateReplacement)

containsLikelyTagStart :: String -> Bool
containsLikelyTagStart [] = False
containsLikelyTagStart ('<':rest) = looksLikeTagStart rest || containsLikelyTagStart rest
containsLikelyTagStart (_:rest) = containsLikelyTagStart rest

rewriteContentHeuristic :: Maybe String -> String -> String
rewriteContentHeuristic fileLevelQQ = go Normal ""
  where
    go _ _ [] = []
    go Normal lineBuf s@(c:cs)
        | c == '-' && isPrefixOf "--" s = "--" ++ go LineComment (lineBuf ++ "--") (drop 2 s)
        | c == '{' && isPrefixOf "{-" s = "{-" ++ go (BlockComment 1) (lineBuf ++ "{-") (drop 2 s)
        | c == '"' = '"' : go StringLit (lineBuf ++ "\"") cs
        | c == '\'' = '\'' : go CharLit (lineBuf ++ "'") cs
        | Just qqName <- fileLevelQQ
        , c == '<'
        , looksLikeTagStart cs
        , shouldStartHsxInLine lineBuf cs =
            let (body, rest) = orDie (consumeHsxBlock qqName s)
                replacement = "$(quoteExp " ++ qqName ++ " " ++ show body ++ ")"
            in replacement ++ go Normal (lineBuf ++ qqName) rest
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

isRequiredHsxCandidate :: String -> [HsxCandidate] -> HsxCandidate -> Bool
isRequiredHsxCandidate input allCandidates candidate =
    case parseModuleWithGhc "<ihp-hsx-pp>" testInput of
        Left _ -> True
        Right _ -> False
  where
    testInput =
        applyCandidateReplacement input allCandidates replacementForCandidate

    replacementForCandidate current
        | candidateId current == candidateId candidate = candidateOriginal current
        | otherwise = candidatePlaceholderName current

collectExpressionPlaceholders :: String -> Located (HsModule GhcPs) -> Set String
collectExpressionPlaceholders prefix moduleAst =
    query moduleAst
  where
    query :: forall a. Data a => a -> Set String
    query value
        | Just (expr :: HsExpr GhcPs) <- cast value = collectFromExpression expr
        | otherwise = Set.unions (gmapQ query value)

    collectFromExpression :: HsExpr GhcPs -> Set String
    collectFromExpression expression =
        let current = case expression of
                HsVar _ (L _ rdrName) ->
                    let occ = occNameString (rdrNameOcc rdrName)
                    in if prefix `isPrefixOf` occ then Set.singleton occ else Set.empty
                _ -> Set.empty
        in current `Set.union` Set.unions (gmapQ query expression)

parseModuleWithGhc :: FilePath -> String -> Either String (Located (HsModule GhcPs))
parseModuleWithGhc sourceName input =
    case GHCLexer.unP GHCParser.parseModule parseState of
        POk _ result -> Right result
        PFailed parserState -> Left (renderParserError parserState)
  where
    location = mkRealSrcLoc (mkFastString sourceName) 1 1
    buffer = stringToStringBuffer input
    parseState = GHCLexer.initParserState parserOpts buffer location

renderParserError :: PState -> String
renderParserError parserState =
    renderWithContext defaultSDocContext
        $ vcat
#if __GLASGOW_HASKELL__ >= 908
        $ map formatBulleted
#else
        $ map (formatBulleted defaultSDocContext)
#endif
#if __GLASGOW_HASKELL__ >= 906
        $ map (diagnosticMessage NoDiagnosticOpts)
#else
        $ map diagnosticMessage
#endif
        $ map errMsgDiagnostic
        $ sortMsgBag Nothing
        $ getMessages (GHCLexer.errors parserState)

parserOpts :: GHCLexer.ParserOpts
parserOpts = GHCLexer.mkParserOpts (EnumSet.empty) diagOpts [] False False False False

diagOpts :: DiagOpts
diagOpts =
    DiagOpts
        { diag_warning_flags = EnumSet.empty
        , diag_fatal_warning_flags = EnumSet.empty
        , diag_warn_is_error = False
        , diag_reverse_errors = False
        , diag_max_errors = Nothing
        , diag_ppr_ctx = defaultSDocContext
#if __GLASGOW_HASKELL__ >= 908
        , diag_custom_warning_categories = emptyWarningCategorySet
        , diag_fatal_custom_warning_categories = emptyWarningCategorySet
#endif
        }

data HsxCandidate = HsxCandidate
    { candidateId :: !Int
    , candidateStart :: !Int
    , candidateEnd :: !Int
    , candidateOriginal :: !String
    , candidateReplacement :: !String
    , candidatePlaceholderName :: !String
    }
    deriving (Eq, Show)

assignPlaceholderNames :: String -> [HsxCandidate] -> [HsxCandidate]
assignPlaceholderNames prefix =
    zipWith assign [1 :: Int ..]
  where
    assign idx candidate =
        candidate
            { candidatePlaceholderName = prefix ++ show idx
            }

pickPlaceholderPrefix :: String -> String
pickPlaceholderPrefix input =
    head (Prelude.dropWhile (`isInfixOf` input) prefixes)
  where
    base = "__ihp_hsx_pp_placeholder_"
    prefixes = [base ++ replicate n '_' | n <- [0 :: Int ..]]

applyCandidateReplacement :: String -> [HsxCandidate] -> (HsxCandidate -> String) -> String
applyCandidateReplacement input candidates replacementFor =
    go 0 (sortOn candidateStart candidates)
  where
    go offset [] = drop offset input
    go offset (candidate:rest) =
        let prefixChunk = take (candidateStart candidate - offset) (drop offset input)
            replacement = replacementFor candidate
        in prefixChunk ++ replacement ++ go (candidateEnd candidate) rest

detectHsxCandidates :: String -> String -> [HsxCandidate]
detectHsxCandidates fileLevelQQ input =
    reverse (go Normal 0 1 [] input)
  where
    go _ _ _ acc [] = acc
    go Normal offset nextId acc s@(c:cs)
        | c == '-' && isPrefixOf "--" s = go LineComment (offset + 2) nextId acc (drop 2 s)
        | c == '{' && isPrefixOf "{-" s = go (BlockComment 1) (offset + 2) nextId acc (drop 2 s)
        | c == '"' = go StringLit (offset + 1) nextId acc cs
        | c == '\'' = go CharLit (offset + 1) nextId acc cs
        | c == '[' =
            case parseQQStart cs of
                Just (qqName, restAfterBar) ->
                    case consumeQQRaw restAfterBar of
                        Right (body, rest) ->
                            let consumedLen = 1 + length qqName + 1 + length body + 2
                            in go Normal (offset + consumedLen) nextId acc rest
                        Left _ ->
                            go Normal (offset + 1) nextId acc cs
                Nothing ->
                    go Normal (offset + 1) nextId acc cs
        | c == '<'
        , looksLikeTagStart cs
        , looksLikeCompleteTagPrefix cs =
            case consumeHsxBlock fileLevelQQ s of
                Right (body, rest) ->
                    let consumedLen = length s - length rest
                        original = take consumedLen s
                        replacement = "$(quoteExp " ++ fileLevelQQ ++ " " ++ show body ++ ")"
                        candidate = HsxCandidate
                            { candidateId = nextId
                            , candidateStart = offset
                            , candidateEnd = offset + consumedLen
                            , candidateOriginal = original
                            , candidateReplacement = replacement
                            , candidatePlaceholderName = ""
                            }
                    in go Normal (offset + consumedLen) (nextId + 1) (candidate : acc) rest
                Left _ ->
                    go Normal (offset + 1) nextId acc cs
        | otherwise = go Normal (offset + 1) nextId acc cs
    go LineComment offset nextId acc s@(c:cs)
        | c == '\n' = go Normal (offset + 1) nextId acc cs
        | otherwise = go LineComment (offset + 1) nextId acc cs
    go (BlockComment depth) offset nextId acc s
        | isPrefixOf "{-" s = go (BlockComment (depth + 1)) (offset + 2) nextId acc (drop 2 s)
        | isPrefixOf "-}" s =
            let depth' = depth - 1
            in if depth' == 0
                then go Normal (offset + 2) nextId acc (drop 2 s)
                else go (BlockComment depth') (offset + 2) nextId acc (drop 2 s)
        | otherwise =
            case s of
                (_:rest) -> go (BlockComment depth) (offset + 1) nextId acc rest
    go StringLit offset nextId acc s@(c:cs)
        | c == '\\' = case cs of
            [] -> go StringLit (offset + 1) nextId acc []
            (_:ds) -> go StringLit (offset + 2) nextId acc ds
        | c == '"' = go Normal (offset + 1) nextId acc cs
        | otherwise = go StringLit (offset + 1) nextId acc cs
    go CharLit offset nextId acc s@(c:cs)
        | c == '\\' = case cs of
            [] -> go CharLit (offset + 1) nextId acc []
            (_:ds) -> go CharLit (offset + 2) nextId acc ds
        | c == '\'' = go Normal (offset + 1) nextId acc cs
        | otherwise = go CharLit (offset + 1) nextId acc cs

data Mode = Normal | LineComment | BlockComment Int | StringLit | CharLit

advanceLineStart :: Bool -> Char -> Bool
advanceLineStart lineStart c
    | c == '\n' = True
    | lineStart && (c == ' ' || c == '\t') = True
    | otherwise = False

consumeQQRaw :: String -> Either String (String, String)
consumeQQRaw = go []
  where
    go _ [] = Left "ihp-hsx-pp: unterminated quasiquote"
    go acc s
        | isPrefixOf "|]" s = Right (reverse acc, drop 2 s)
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

looksLikeTagStart :: String -> Bool
looksLikeTagStart [] = False
looksLikeTagStart (c:_)
    | c == '/' = False
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

consumeHsxBlock :: String -> String -> Either String (String, String)
consumeHsxBlock fileLevelQQ s =
    consumeSingleHsxBlock fileLevelQQ s >>= uncurry consumeSiblingRoots
  where
    consumeSiblingRoots acc rest =
        let (spaces, rest') = span isSpace rest
        in case rest' of
            ('<':tagTail)
                | looksLikeTagStart tagTail ->
                    consumeSingleHsxBlock fileLevelQQ rest'
                        >>= \(nextRoot, rest'') -> consumeSiblingRoots (acc ++ spaces ++ nextRoot) rest''
            _ -> Right (acc, rest)

consumeSingleHsxBlock :: String -> String -> Either String (String, String)
consumeSingleHsxBlock fileLevelQQ s =
    consumeTagText fileLevelQQ s >>= \(tagText, info, rest) ->
        let (stack, rawTag) = updateStack [] Nothing info
        in if null stack
            then Right (tagText, rest)
            else do
                (body, rest') <- consumeHsxBody fileLevelQQ stack rawTag rest
                Right (tagText ++ body, rest')

consumeHsxBody :: String -> [String] -> Maybe String -> String -> Either String (String, String)
consumeHsxBody _ _ _ [] = Left "ihp-hsx-pp: unterminated hsx block"
consumeHsxBody fileLevelQQ stack rawTag s
    | Just tagName <- rawTag =
        let (rawText, rest) = scanUntilClosingTag tagName s
        in case rest of
            [] -> Left "ihp-hsx-pp: unterminated raw-text hsx block"
            _ ->
                consumeTagText fileLevelQQ rest >>= \(tagText, info, rest') ->
                    let (stack', rawTag') = updateStack stack rawTag info
                    in if null stack'
                        then Right (rawText ++ tagText, rest')
                        else do
                            (more, rest'') <- consumeHsxBody fileLevelQQ stack' rawTag' rest'
                            Right (rawText ++ tagText ++ more, rest'')
    | otherwise =
        case s of
            ('{':rest) -> do
                (content, rest') <- consumeBraceRaw rest
                let rewritten = rewriteHaskellSegment fileLevelQQ content
                (more, rest'') <- consumeHsxBody fileLevelQQ stack rawTag rest'
                Right ("{" ++ rewritten ++ "}" ++ more, rest'')
            ('<':_) ->
                consumeTagText fileLevelQQ s >>= \(tagText, info, rest') ->
                    let (stack', rawTag') = updateStack stack rawTag info
                    in if null stack'
                        then Right (tagText, rest')
                        else do
                            (more, rest'') <- consumeHsxBody fileLevelQQ stack' rawTag' rest'
                            Right (tagText ++ more, rest'')
            (c:cs) ->
                consumeHsxBody fileLevelQQ stack rawTag cs >>= \(more, rest') ->
                    Right (c:more, rest')

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

consumeTagText :: String -> String -> Either String (String, TagInfo, String)
consumeTagText _ s@('<':'!':'-':'-':rest) =
    let (body, rest') = breakOn "-->" rest
    in Right ("<!--" ++ body ++ "-->", TagSpecial, rest')
consumeTagText fileLevelQQ s@('<':rest) =
    consumeTagBody fileLevelQQ rest >>= \(body, rest') ->
        let tagText = "<" ++ body
            info = classifyTag tagText
        in Right (tagText, info, rest')
consumeTagText _ _ = Left "ihp-hsx-pp: expected tag start"

consumeTagBody :: String -> String -> Either String (String, String)
consumeTagBody fileLevelQQ = go Nothing []
  where
    go _ _ [] = Left "ihp-hsx-pp: unterminated tag"
    go quote acc s@(c:cs)
        | quote == Nothing && c == '"' = go (Just '"') (c:acc) cs
        | quote == Just '"' && c == '"' = go Nothing (c:acc) cs
        | quote == Nothing && c == '\'' = go (Just '\'') (c:acc) cs
        | quote == Just '\'' && c == '\'' = go Nothing (c:acc) cs
        | quote == Nothing && c == '{' = do
            (content, rest) <- consumeBraceRaw cs
            let rewritten = rewriteHaskellSegment fileLevelQQ content
            let acc' = pushString ("{" ++ rewritten ++ "}") acc
            go Nothing acc' rest
        | quote == Nothing && c == '>' = Right (reverse ('>':acc), cs)
        | otherwise = go quote (c:acc) cs

classifyTag :: String -> TagInfo
classifyTag tagText
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

rewriteHaskellSegment :: String -> String -> String
rewriteHaskellSegment fileLevelQQ = go Normal ""
  where
    go _ _ [] = []
    go Normal lineBuf s@(c:cs)
        | c == '-' && isPrefixOf "--" s = "--" ++ go LineComment (lineBuf ++ "--") (drop 2 s)
        | c == '{' && isPrefixOf "{-" s = "{-" ++ go (BlockComment 1) (lineBuf ++ "{-") (drop 2 s)
        | c == '"' = '"' : go StringLit (lineBuf ++ "\"") cs
        | c == '\'' = '\'' : go CharLit (lineBuf ++ "'") cs
        | c == '<' && looksLikeTagStart cs && shouldStartHsxInLine lineBuf cs =
            let (body, rest) = orDie (consumeHsxBlock fileLevelQQ s)
                replacement = "$(quoteExp " ++ fileLevelQQ ++ " " ++ show body ++ ")"
            in replacement ++ go Normal (lineBuf ++ fileLevelQQ) rest
        | c == '[' =
            case parseQQStart cs of
                Just (qqName, restAfterBar) ->
                    let (body, rest) = orDie (consumeQQRaw restAfterBar)
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

orDie :: Either String a -> a
orDie =
    either error id

shouldStartHsxInLine :: String -> String -> Bool
shouldStartHsxInLine lineBuf tagTail =
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
        || isApplicationContext lineBuf trimmed tagTail

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

isApplicationContext :: String -> String -> String -> Bool
isApplicationContext lineBuf trimmed tagTail =
    hasWhitespaceBeforeTag lineBuf
        && not (null trimmed)
        && endsWithApplicationToken trimmed
        && looksLikeCompleteTagPrefix tagTail

hasWhitespaceBeforeTag :: String -> Bool
hasWhitespaceBeforeTag [] = False
hasWhitespaceBeforeTag s =
    isSpace (last s)

endsWithApplicationToken :: String -> Bool
endsWithApplicationToken trimmed =
    case reverse trimmed of
        (c:_) | c `elem` (")]}\"'" :: String) -> True
        _ -> endsWithIdentifierToken trimmed || endsWithOperatorToken trimmed

endsWithIdentifierToken :: String -> Bool
endsWithIdentifierToken s =
    let token = takeWhileEnd isIdentifierTokenChar s
    in isQualifiedIdentifier token
  where
    isIdentifierTokenChar c = isIdentChar c || c == '.'

endsWithOperatorToken :: String -> Bool
endsWithOperatorToken s =
    let token = takeWhileEnd isOperatorTokenChar s
    in not (null token)

isOperatorTokenChar :: Char -> Bool
isOperatorTokenChar c =
    c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

takeWhileEnd :: (Char -> Bool) -> String -> String
takeWhileEnd predicate = reverse . takeWhile predicate . reverse

looksLikeCompleteTagPrefix :: String -> Bool
looksLikeCompleteTagPrefix s =
    case span isTagNameChar s of
        ("", _) -> False
        (_, rest) ->
            case rest of
                ('>':_) -> True
                ('/':'>':_) -> True
                (c:_) | isSpace c -> '>' `elem` rest
                _ -> False

isQualifiedIdentifier :: String -> Bool
isQualifiedIdentifier token =
    not (null token)
        && all isIdentifierSegment (splitOnDot token)
  where
    splitOnDot [] = [""]
    splitOnDot value =
        case break (== '.') value of
            (segment, []) -> [segment]
            (segment, _:rest) -> segment : splitOnDot rest

    isIdentifierSegment [] = False
    isIdentifierSegment (x:xs) = isIdentStart x && all isIdentChar xs

updateLineBuf :: String -> String -> String
updateLineBuf lineBuf raw =
    case elemIndex '\n' (reverse raw) of
        Nothing -> lineBuf ++ raw
        Just idxFromEnd ->
            let idx = length raw - idxFromEnd - 1
            in drop (idx + 1) raw

consumeBraceRaw :: String -> Either String (String, String)
consumeBraceRaw = go 1 Normal []
  where
    go _ _ _ [] = Left "ihp-hsx-pp: unterminated { } splice"
    go depth mode acc s
        | depth == 0 = Right (reverse acc, s)
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
                                        then Right (reverse acc, cs)
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
