{-# LANGUAGE OverloadedStrings #-}

module IHP.HSX.Format.Locate
    ( locateQuotes
    ) where

import Prelude
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import IHP.HSX.Format.Types

locateQuotes :: Text -> [QuoteRegion]
locateQuotes source = go 0 Normal (Text.unpack source)
    where
        go !index !state input = case state of
            Normal -> case input of
                '-':'-':rest -> go (index + 2) LineComment rest
                '{':'-':rest -> go (index + 2) (BlockComment 1) rest
                '"':rest -> go (index + 1) StringLiteral rest
                '\'':rest -> go (index + 1) CharLiteral rest
                '[':rest ->
                    case parseQuasiQuoteName rest of
                        Just (quoteName, consumed)
                            | quoteName `elem` supportedQuasiQuoters ->
                                let bodyStart = index + 1 + consumed
                                    quoteBody = drop consumed rest
                                in case findQuoteEnd bodyStart quoteBody of
                                    Just (bodyEnd, endPos) ->
                                        quoteRegion quoteName index bodyStart bodyEnd endPos : go endPos Normal (drop (endPos - bodyStart) quoteBody)
                                    Nothing -> []
                        _ -> go (index + 1) Normal rest
                [] -> []
                (_:rest) -> go (index + 1) Normal rest
            LineComment -> case input of
                '\n':rest -> go (index + 1) Normal rest
                [] -> []
                (_:rest) -> go (index + 1) LineComment rest
            BlockComment !depth -> case input of
                '{':'-':rest -> go (index + 2) (BlockComment (depth + 1)) rest
                '-':'}':rest ->
                    if depth == 1
                        then go (index + 2) Normal rest
                        else go (index + 2) (BlockComment (depth - 1)) rest
                [] -> []
                (_:rest) -> go (index + 1) (BlockComment depth) rest
            StringLiteral -> case input of
                '\\':_:rest -> go (index + 2) StringLiteral rest
                '"':rest -> go (index + 1) Normal rest
                [] -> []
                (_:rest) -> go (index + 1) StringLiteral rest
            CharLiteral -> case input of
                '\\':_:rest -> go (index + 2) CharLiteral rest
                '\'':rest -> go (index + 1) Normal rest
                [] -> []
                (_:rest) -> go (index + 1) CharLiteral rest

        quoteRegion quoteName start bodyStart bodyEnd endPos =
            QuoteRegion
                { name = Text.pack quoteName
                , startIndex = start
                , bodyStartIndex = bodyStart
                , bodyEndIndex = bodyEnd
                , endIndex = endPos
                , linePrefix = findLinePrefix source start
                }

supportedQuasiQuoters :: [String]
supportedQuasiQuoters =
    [ "hsx"
    , "uncheckedHsx"
    , "hsxM"
    , "uncheckedHsxM"
    ]

data ScannerState
    = Normal
    | LineComment
    | BlockComment !Int
    | StringLiteral
    | CharLiteral

parseQuasiQuoteName :: String -> Maybe (String, Int)
parseQuasiQuoteName input =
    let (name, rest) = span isIdentifierChar input
    in case rest of
        '|':_ | not (null name) -> Just (name, length name + 1)
        _ -> Nothing

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_' || c == '\''

findQuoteEnd :: Int -> String -> Maybe (Int, Int)
findQuoteEnd !currentIndex input = go currentIndex input
    where
        go !index chars = case chars of
            '|':']':_ -> Just (index, index + 2)
            [] -> Nothing
            (_:rest) -> go (index + 1) rest

findLinePrefix :: Text -> Int -> Text
findLinePrefix source start =
    let beforeQuote = Text.take start source
        lineText = case Text.splitOn "\n" beforeQuote of
            [] -> ""
            xs -> last xs
    in Text.takeWhile isSpace lineText
