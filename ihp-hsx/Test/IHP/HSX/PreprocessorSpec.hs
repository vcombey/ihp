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

    it "allows selecting a custom file-level quasiquoter in the marker" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx myHsx"
            , "module View where"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = $(quoteExp myHsx \"<div>Hello</div>\")"

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

    it "allows selecting a custom file-level quasiquoter via options" do
        output <- runPreprocessorOnWithOptions ["--hsx-qq=myHsx"] "View.hsx" (unlines
            [ "module View where"
            , "view = <div>Hello</div>"
            ])

        output `shouldContain` "view = $(quoteExp myHsx \"<div>Hello</div>\")"
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

    it "rewrites sibling root tags as a single file-level expression" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "stylesheets ="
            , "    <link rel=\"stylesheet\" href=\"/a.css\"/>"
            , "    <link rel=\"stylesheet\" href=\"/b.css\"/>"
            ])

        countOccurrences "$(quoteExp hsx" output `shouldBe` 1
        output `shouldContain` "<link rel=\\\"stylesheet\\\" href=\\\"/a.css\\\"/>"
        output `shouldContain` "<link rel=\\\"stylesheet\\\" href=\\\"/b.css\\\"/>"

    it "does not rewrite hsx quasiquotes in normal files" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "module View where"
            , "view = [hsx|<div>Hello</div>|]"
            ])

        output `shouldContain` "view = [hsx|<div>Hello</div>|]"
        output `shouldNotContain` "$(quoteExp hsx \"<div>Hello</div>\")"
        output `shouldNotContain` "{-# LANGUAGE TemplateHaskell #-}"
        output `shouldNotContain` "import Language.Haskell.TH.Quote (quoteExp)"

    it "does not rewrite plain haskell operators as tags" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "isSmall x = x < 10"
            ])

        output `shouldContain` "isSmall x = x < 10"
        output `shouldNotContain` "$(quoteExp hsx \"< 10\")"

    it "rewrites file-level HSX in function application position" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "view = pure <div>Hello</div>"
            ])

        output `shouldContain` "view = pure $(quoteExp hsx \"<div>Hello</div>\")"

    it "rewrites file-level HSX after a dollar operator" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "view = pure $ <div>Hello</div>"
            ])

        output `shouldContain` "view = pure $ $(quoteExp hsx \"<div>Hello</div>\")"

    it "rewrites file-level HSX after infix operators" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "view = render <$> <div>Hello</div>"
            ])

        output `shouldContain` "view = render <$> $(quoteExp hsx \"<div>Hello</div>\")"

    it "rewrites nested file-level HSX in function application position inside splices" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "view = <div>{pure <span>Nested</span>}</div>"
            ])

        output `shouldContain` "{pure $(quoteExp hsx \\\"<span>Nested</span>\\\")}"

    it "does not mis-detect compact comparisons as hsx tags" do
        output <- runPreprocessorOn "View.hs" (unlines
            [ "-- hsx"
            , "module View where"
            , "isLess x y = x <y"
            ])

        output `shouldContain` "isLess x y = x <y"
        output `shouldNotContain` "$(quoteExp hsx \"<y\")"

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

runPreprocessorOn :: FilePath -> String -> IO String
runPreprocessorOn fileName inputContent =
    runPreprocessorOnWithOptions [] fileName inputContent

runPreprocessorOnWithOptions :: [String] -> FilePath -> String -> IO String
runPreprocessorOnWithOptions options fileName inputContent =
    withSystemTempDirectory "ihp-hsx-pp-spec" \dir -> do
        let inputPath = dir </> fileName
        writeFile inputPath inputContent
        runPreprocessorWithOptions options inputPath

runPreprocessorFailureOn :: FilePath -> String -> IO String
runPreprocessorFailureOn fileName inputContent =
    runPreprocessorFailureOnWithOptions [] fileName inputContent

runPreprocessorFailureOnWithOptions :: [String] -> FilePath -> String -> IO String
runPreprocessorFailureOnWithOptions options fileName inputContent =
    withSystemTempDirectory "ihp-hsx-pp-spec" \dir -> do
        let inputPath = dir </> fileName
        writeFile inputPath inputContent
        runPreprocessorFailureWithOptions options inputPath

runPreprocessor :: FilePath -> IO String
runPreprocessor = runPreprocessorWithOptions []

runPreprocessorWithOptions :: [String] -> FilePath -> IO String
runPreprocessorWithOptions options inputPath = do
    (exitCode, _, stdErr, output) <- runPreprocessorRawWithOptions options inputPath
    case (exitCode, output) of
        (ExitSuccess, Just result) -> pure result
        (ExitSuccess, Nothing) -> fail "ihp-hsx-pp succeeded without writing an output file"
        (ExitFailure code, _) -> fail ("ihp-hsx-pp failed with exit code " <> show code <> ":\n" <> stdErr)

runPreprocessorFailure :: FilePath -> IO String
runPreprocessorFailure = runPreprocessorFailureWithOptions []

runPreprocessorFailureWithOptions :: [String] -> FilePath -> IO String
runPreprocessorFailureWithOptions options inputPath = do
    (exitCode, _, stdErr, _) <- runPreprocessorRawWithOptions options inputPath
    case exitCode of
        ExitFailure _ -> pure stdErr
        ExitSuccess -> fail "ihp-hsx-pp succeeded, but a failure was expected"

runPreprocessorRaw :: FilePath -> IO (ExitCode, String, String, Maybe String)
runPreprocessorRaw = runPreprocessorRawWithOptions []

runPreprocessorRawWithOptions :: [String] -> FilePath -> IO (ExitCode, String, String, Maybe String)
runPreprocessorRawWithOptions options inputPath =
    withSystemTempFile "ihp-hsx-pp-output.hs" \outputPath handle -> do
        hClose handle
        executable <- resolvePreprocessorExecutable
        (exitCode, stdOut, stdErr) <- readProcessWithExitCode executable (options <> [inputPath, outputPath]) ""
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
