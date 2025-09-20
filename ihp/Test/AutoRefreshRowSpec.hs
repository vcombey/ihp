{-|
Module: Test.AutoRefreshRowSpec

Lightweight tests for row-level AutoRefresh plumbing.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.AutoRefreshRowSpec where

import Test.Hspec
import IHP.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

import IHP.ModelSupport (withRowReadTracker, trackRowRead, notConnectedModelContext)
import qualified IHP.Log.Types as Log
import IHP.AutoRefresh (..)

tests :: Spec
tests = describe "AutoRefresh Row" do
    it "decodes RowChange JSON payloads" do
        (Aeson.decode "{\"id\":\"abc\"}" :: Maybe RowChange) `shouldBe` Just (RowChange "abc")
        (Aeson.decode "{\"id\":123}" :: Maybe RowChange) `shouldBe` Just (RowChange "123")
        (Aeson.decode "{\"id\":true}" :: Maybe RowChange) `shouldBe` Just (RowChange "true")

    it "tracks row ids within withRowReadTracker" do
        logger <- Log.defaultLogger
        let ?modelContext = notConnectedModelContext logger
        withRowReadTracker do
            trackRowRead "posts" "row-1"
            m <- readIORef ?touchedRowIds
            HashMap.lookup "posts" m `shouldSatisfy` \case
                Nothing -> False
                Just s  -> Set.member "row-1" s
