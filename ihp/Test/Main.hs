module Main where

import IHP.Prelude

import Test.Hspec
import qualified AutoRefreshSpec

main :: IO ()
main = hspec do
    AutoRefreshSpec.spec
