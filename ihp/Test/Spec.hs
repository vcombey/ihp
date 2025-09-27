module Main where

import Test.Hspec
import qualified AutoRefreshSpec

main :: IO ()
main = hspec do
    AutoRefreshSpec.spec
