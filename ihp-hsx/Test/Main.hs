module Main where

import Prelude

import Test.Hspec
import qualified IHP.HSX.QQSpec
import qualified IHP.HSX.ParserSpec
import qualified IHP.HSX.FileLevelSpec
import qualified IHP.HSX.HaskellParserSpec
import qualified IHP.HSX.PreprocessorSpec

main :: IO ()
main = hspec do
    IHP.HSX.QQSpec.tests
    IHP.HSX.ParserSpec.tests
    IHP.HSX.FileLevelSpec.tests
    IHP.HSX.HaskellParserSpec.tests
    IHP.HSX.PreprocessorSpec.tests
