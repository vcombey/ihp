{-|
Module: IHP.LocalFirst.Safety
Description: Lightweight safety checks for local-first controller actions
Copyright: (c) digitally induced GmbH, 2026
-}
module IHP.LocalFirst.Safety where

import IHP.Prelude
import qualified Data.Text as Text

data LocalSafetyViolation = LocalSafetyViolation
    { filePath :: !FilePath
    , line :: !Int
    , message :: !Text
    } deriving (Eq, Show)

unsafeAPIs :: [(Text, Text)]
unsafeAPIs =
    [ ("httpLBS", "Network requests are not allowed in local actions")
    , ("wreq", "Network requests are not allowed in local actions")
    , ("createProcess", "Process spawning is not allowed in local actions")
    , ("readProcess", "Process spawning is not allowed in local actions")
    , ("runJob", "Background jobs are not allowed in local actions")
    , ("sqlQuery", "Raw sqlQuery usage inside local actions is not supported")
    , ("sqlExec", "Raw sqlExec usage inside local actions is not supported")
    ]

scanLocalSafetySource :: FilePath -> Text -> [LocalSafetyViolation]
scanLocalSafetySource sourceFile source =
    concatMap inspectLine (zip [1 ..] (Text.lines source))
    where
        inspectLine (lineNumber, lineText)
            | isCommentLine lineText = []
            | otherwise =
                unsafeAPIs
                    |> mapMaybe (\(needle, description) ->
                        if needle `Text.isInfixOf` lineText && appearsInLocalContext lineNumber
                            then Just LocalSafetyViolation
                                { filePath = sourceFile
                                , line = lineNumber
                                , message = description <> " (" <> needle <> ")"
                                }
                            else Nothing
                    )

        sourceLines = Text.lines source

        appearsInLocalContext lineNumber =
            let
                previousLines = sourceLines |> take lineNumber |> reverse
                window = take 30 previousLines
            in
                any (\l -> "local do" `Text.isInfixOf` l || "localWith" `Text.isInfixOf` l) window

        isCommentLine lineText =
            let trimmed = Text.strip lineText
            in "--" `Text.isPrefixOf` trimmed

hasLocalSafetyViolations :: FilePath -> Text -> Bool
hasLocalSafetyViolations sourceFile source = not (null (scanLocalSafetySource sourceFile source))

