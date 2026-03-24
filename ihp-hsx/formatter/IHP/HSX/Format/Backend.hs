{-# LANGUAGE OverloadedStrings #-}

module IHP.HSX.Format.Backend
    ( Backend (..)
    , formatExpressionWithBackend
    , runSourceBackend
    ) where

import Prelude
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

data Backend
    = NoBackend
    | Ormolu
    | Fourmolu
    deriving (Eq, Show)

runSourceBackend :: Backend -> FilePath -> Text -> IO (Either Text Text)
runSourceBackend NoBackend _ input = pure (Right input)
runSourceBackend backend sourcePath input = do
    let (command, args) = backendCommand backend sourcePath
    (exitCode, stdoutText, stderrText) <- readProcessWithExitCode command args (Text.unpack input)
    pure case exitCode of
        ExitSuccess -> Right (Text.pack stdoutText)
        ExitFailure _ ->
            Left ("Backend " <> Text.pack command <> " failed:\n" <> Text.pack stderrText)

formatExpressionWithBackend :: Backend -> FilePath -> [Text] -> Text -> IO (Either Text Text)
formatExpressionWithBackend NoBackend _ _ expression = pure (Right (Text.strip expression))
formatExpressionWithBackend backend sourcePath pragmas expression = do
    let markerBegin = "HSXFMT_BEGIN_MARKER"
    let markerEnd = "HSXFMT_END_MARKER"
    let wrapper = buildExpressionWrapper pragmas markerBegin markerEnd expression
    backendResult <- runSourceBackend backend sourcePath wrapper
    pure do
        formattedWrapper <- backendResult
        extractFormattedExpression markerBegin markerEnd formattedWrapper

backendCommand :: Backend -> FilePath -> (FilePath, [String])
backendCommand backend sourcePath = case backend of
    NoBackend -> ("cat", [])
    Ormolu -> ("ormolu", ["--stdin-input-file", sourcePath])
    Fourmolu -> ("fourmolu", ["--stdin-input-file", sourcePath])

buildExpressionWrapper :: [Text] -> Text -> Text -> Text -> Text
buildExpressionWrapper pragmas markerBegin markerEnd expression =
    Text.unlines $
        pragmas
            <> [ "module HsxfmtTemporary where"
               , ""
               , "__hsxfmt__ ="
               , "    {- " <> markerBegin <> " -}"
               ]
            <> map ("    " <>) (Text.lines (Text.strip expression))
            <> [ "    {- " <> markerEnd <> " -}" ]

extractFormattedExpression :: Text -> Text -> Text -> Either Text Text
extractFormattedExpression markerBegin markerEnd formattedWrapper = do
    let lines' = Text.lines formattedWrapper
    beginIndex <- findMarkerIndex markerBegin lines'
    endIndex <- findMarkerIndex markerEnd lines'
    if endIndex <= beginIndex
        then Left "Could not recover formatted expression from backend output"
        else
            let expressionLines = take (endIndex - beginIndex - 1) (drop (beginIndex + 1) lines')
            in Right (normalizeExpressionBlock (Text.intercalate "\n" expressionLines))

findMarkerIndex :: Text -> [Text] -> Either Text Int
findMarkerIndex marker lines' = case findIndex 0 lines' of
    Just index -> Right index
    Nothing -> Left ("Missing formatter marker: " <> marker)
    where
        needle = "{- " <> marker <> " -}"
        findIndex _ [] = Nothing
        findIndex index (line:rest)
            | Text.strip line == needle = Just index
            | otherwise = findIndex (index + 1) rest

normalizeExpressionBlock :: Text -> Text
normalizeExpressionBlock =
    stripCommonIndent
        . Text.intercalate "\n"
        . reverse
        . dropWhile (Text.all isSpace)
        . reverse
        . dropWhile (Text.all isSpace)
        . Text.lines

stripCommonIndent :: Text -> Text
stripCommonIndent value =
    let lines' = Text.lines value
        nonBlank = filter (not . Text.all isSpace) lines'
        smallestIndent = minimumSafe (map (Text.length . Text.takeWhile isSpace) nonBlank)
    in case smallestIndent of
        Nothing -> Text.strip value
        Just indentSize ->
            Text.intercalate "\n" (map (Text.drop indentSize) lines')

minimumSafe :: [Int] -> Maybe Int
minimumSafe [] = Nothing
minimumSafe (x:xs) = Just (foldl min x xs)
