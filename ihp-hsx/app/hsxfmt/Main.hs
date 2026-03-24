{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import Data.Text (Text)
import qualified Data.Text.IO as Text
import IHP.HSX.Format
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.IO as IO

main :: IO ()
main = do
    arguments <- getArgs
    case parseArguments arguments of
        Left errorMessage -> do
            IO.hPutStrLn IO.stderr errorMessage
            IO.hPutStrLn IO.stderr usage
            exitFailure
        Right cliOptions -> runCli cliOptions

data CliOptions = CliOptions
    { inputFile :: !(Maybe FilePath)
    , selectedBackend :: !Backend
    , readFromStdin :: !Bool
    , inplace :: !Bool
    }

parseArguments :: [String] -> Either String CliOptions
parseArguments = go defaultCliOptions
    where
        go cliOptions arguments = case arguments of
            [] -> case cliOptions.inputFile of
                Just _ -> Right cliOptions
                Nothing -> Left "Missing input path. Use a file path or --stdin-input-file."
            ["--help"] -> Left ""
            "--stdin-input-file":path:rest -> go (cliOptions { inputFile = Just path, readFromStdin = True }) rest
            "--backend":"ormolu":rest -> go (cliOptions { selectedBackend = Ormolu }) rest
            "--backend":"fourmolu":rest -> go (cliOptions { selectedBackend = Fourmolu }) rest
            "--backend":"none":rest -> go (cliOptions { selectedBackend = NoBackend }) rest
            "--inplace":rest -> go (cliOptions { inplace = True }) rest
            option:_ | Prelude.take 2 option == "--" -> Left ("Unknown option: " <> option)
            path:rest -> go (cliOptions { inputFile = Just path, readFromStdin = False }) rest

defaultCliOptions :: CliOptions
defaultCliOptions =
    CliOptions
        { inputFile = Nothing
        , selectedBackend = NoBackend
        , readFromStdin = False
        , inplace = False
        }

runCli :: CliOptions -> IO ()
runCli CliOptions { inputFile, selectedBackend, readFromStdin, inplace } = do
    let resolvedInputFile = maybe "<stdin>" id inputFile
    input <- readInput readFromStdin inputFile
    let options = (defaultFormatOptions resolvedInputFile) { backend = selectedBackend }
    formatted <- formatSource options input
    case formatted of
        Left errorMessage -> do
            Text.hPutStrLn IO.stderr errorMessage
            exitFailure
        Right output ->
            if inplace
                then case inputFile of
                    Just path -> Text.writeFile path output
                    Nothing -> do
                        IO.hPutStrLn IO.stderr "--inplace requires a real file path"
                        exitFailure
                else Text.putStr output

readInput :: Bool -> Maybe FilePath -> IO Text
readInput readFromStdin inputFile
    | readFromStdin = Text.getContents
    | Just path <- inputFile = Text.readFile path
    | otherwise = Text.getContents

usage :: String
usage = unlines
    [ "Usage: hsxfmt [--backend ormolu|fourmolu|none] [--stdin-input-file PATH | PATH] [--inplace]"
    , ""
    , "Formats supported HSX quasiquotes and optionally runs Ormolu/Fourmolu on the full file."
    ]
