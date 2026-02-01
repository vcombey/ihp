{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -iTest #-}
module Main where

import Test.Hspec
import IHP.Prelude
import qualified Test.AutoRefreshSpec as AutoRefreshSpec

main :: IO ()
main = hspec AutoRefreshSpec.tests
