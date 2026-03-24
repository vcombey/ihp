module Main where

import Prelude

import Test.Hspec
import qualified IHP.HSX.QQSpec
import qualified IHP.HSX.ParserSpec
import qualified IHP.HSX.FormatterSpec

main :: IO ()
main = hspec do
    IHP.HSX.QQSpec.tests
    IHP.HSX.ParserSpec.tests
    IHP.HSX.FormatterSpec.tests
