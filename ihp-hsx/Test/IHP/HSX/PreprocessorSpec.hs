module IHP.HSX.PreprocessorSpec where

import Prelude
import Test.Hspec
import Control.Monad (filterM)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.FilePath ((</>))
import System.Directory (createFileLink, doesFileExist, findExecutable)
import System.IO (hClose)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

tests :: SpecWith ()
tests = describe "ihp-hsx-pp preprocessor" do
    it "enables file-level mode via a header marker" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = $(quoteExp hsx \"<div>Hello</div>\")"
        output `shouldContain` "{-# LANGUAGE QuasiQuotes #-}"

    it "accepts -- hsx-file as header marker" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx-file"
            , "module View where"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = $(quoteExp hsx \"<div>Hello</div>\")"

    it "accepts header markers with trailing whitespace" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx   "
            , "module View where"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = $(quoteExp hsx \"<div>Hello</div>\")"

    it "does not enable file-level mode when -- hsx appears after module declaration" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "module View where"
            , "-- hsx"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = <div>Hello</div>"
        output `shouldNotContain` "$(quoteExp hsx \"<div>Hello</div>\")"
        output `shouldNotContain` "{-# LANGUAGE QuasiQuotes #-}"

    it "does not treat -- hsx inside a header block comment as a marker" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "{-"
            , "-- hsx"
            , "-}"
            , "module View where"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = <div>Hello</div>"
        output `shouldNotContain` "$(quoteExp hsx \"<div>Hello</div>\")"
        output `shouldNotContain` "{-# LANGUAGE QuasiQuotes #-}"

    it "enables file-level mode for .hsx files without markers" do
        output <- runPreprocessorOn "View.hsx" (unlines
            [ "module View where"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = $(quoteExp hsx \"<div>Hello</div>\")"
        output `shouldContain` "{-# LANGUAGE QuasiQuotes #-}"

    it "enables .hsx mode for .hs symlinks pointing to .hsx targets" do
        withSystemTempDirectory "ihp-hsx-pp-symlink" \dir -> do
            let targetPath = dir </> "View.hsx"
            let linkPath = dir </> "View.hs"
            writeFile targetPath (unlines
                [ "module View where"
                , "view = <div>Hello</div>"
                ])
            -- Relative target mirrors the shim setup used by IHP startup.
            createFileLink "View.hsx" linkPath

            output <- runPreprocessor linkPath
            output `shouldContain` "view = $(quoteExp hsx \"<div>Hello</div>\")"
            output `shouldContain` "{-# LANGUAGE QuasiQuotes #-}"

    it "rewrites nested HSX in file-level splices" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "view ="
            , "    <div>"
            , "        {if True then <span>Admin</span> else <span>User</span>}"
            , "    </div>"
            ])

        output `shouldContain` "$(quoteExp hsx \\\"<span>Admin</span>\\\")"
        output `shouldContain` "$(quoteExp hsx \\\"<span>User</span>\\\")"

    it "does not rewrite plain haskell operators as tags" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "isSmall x = x < 10"
            ])

        output `shouldContain` "isSmall x = x < 10"
        output `shouldNotContain` "$(quoteExp hsx \"< 10\")"

    it "respects header pragmas/comments before module and still detects marker" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "{-# OPTIONS_GHC -Wall #-}"
            , "-- comment"
            , "-- hsx"
            , "module View where"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = $(quoteExp hsx \"<div>Hello</div>\")"

    it "does not duplicate quoteExp import when already present" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "import Language.Haskell.TH.Quote (quoteExp)"
            , "view = <div>Hello</div>"
            ])

        countOccurrences "import Language.Haskell.TH.Quote (quoteExp)" output `shouldBe` 1

    it "fails with a clear error on unterminated hsx blocks" do
        stdErr <- runPreprocessorFailureOn "Bad.hs" (unlines
            [ "-- hsx"
            , "module Bad where"
            , "view = <div>"
            ])
        stdErr `shouldContain` "unterminated hsx block"

    it "fails with a clear error on unterminated { } splices" do
        stdErr <- runPreprocessorFailureOn "Bad.hs" (unlines
            [ "-- hsx"
            , "module Bad where"
            , "view ="
            , "    <div>"
            , "        {if True then <span>A</span> else <span>B</span>"
            , "    </div>"
            ])
        stdErr `shouldContain` "unterminated { } splice"

    it "fails with a clear error on unterminated hsx quasiquotes" do
        stdErr <- runPreprocessorFailureOn "Bad.hs" (unlines
            [ "module Bad where"
            , "view = [hsx|<div>"
            ])
        stdErr `shouldContain` "unterminated hsx quasiquote"

runPreprocessorOn :: FilePath -> String -> IO String
runPreprocessorOn fileName inputContent =
    withSystemTempDirectory "ihp-hsx-pp-spec" \dir -> do
        let inputPath = dir </> fileName
        writeFile inputPath inputContent
        runPreprocessor inputPath

runPreprocessorFailureOn :: FilePath -> String -> IO String
runPreprocessorFailureOn fileName inputContent =
    withSystemTempDirectory "ihp-hsx-pp-spec" \dir -> do
        let inputPath = dir </> fileName
        writeFile inputPath inputContent
        runPreprocessorFailure inputPath

runPreprocessor :: FilePath -> IO String
runPreprocessor inputPath = do
    (exitCode, _, stdErr, output) <- runPreprocessorRaw inputPath
    case (exitCode, output) of
        (ExitSuccess, Just result) -> pure result
        (ExitSuccess, Nothing) -> fail "ihp-hsx-pp succeeded without writing an output file"
        (ExitFailure code, _) -> fail ("ihp-hsx-pp failed with exit code " <> show code <> ":\n" <> stdErr)

runPreprocessorFailure :: FilePath -> IO String
runPreprocessorFailure inputPath = do
    (exitCode, _, stdErr, _) <- runPreprocessorRaw inputPath
    case exitCode of
        ExitFailure _ -> pure stdErr
        ExitSuccess -> fail "ihp-hsx-pp succeeded, but a failure was expected"

runPreprocessorRaw :: FilePath -> IO (ExitCode, String, String, Maybe String)
runPreprocessorRaw inputPath =
    withSystemTempFile "ihp-hsx-pp-output.hs" \outputPath handle -> do
        hClose handle
        executable <- resolvePreprocessorExecutable
        (exitCode, stdOut, stdErr) <- readProcessWithExitCode executable [inputPath, outputPath] ""
        output <- case exitCode of
            ExitSuccess -> Just <$> readFile outputPath
            ExitFailure _ -> pure Nothing
        pure (exitCode, stdOut, stdErr, output)

resolvePreprocessorExecutable :: IO FilePath
resolvePreprocessorExecutable = do
    inPath <- findExecutable "ihp-hsx-pp"
    case inPath of
        Just executable -> pure executable
        Nothing -> do
            let localCandidates =
                    [ "dist/build/ihp-hsx-pp/ihp-hsx-pp"
                    , "dist/build/ihp-hsx-pp/ihp-hsx-pp.exe"
                    ]
            existingCandidates <- filterM doesFileExist localCandidates
            case listToMaybe existingCandidates of
                Just executable -> pure executable
                Nothing ->
                    fail "Cannot find ihp-hsx-pp executable (neither on PATH nor in dist/build)."

countOccurrences :: String -> String -> Int
countOccurrences needle haystack
    | null needle = 0
    | otherwise = go haystack 0
  where
    needleLength = length needle
    go [] !acc = acc
    go input@(_:rest) !acc
        | needle `isPrefixOf` input = go (drop needleLength input) (acc + 1)
        | otherwise = go rest acc
