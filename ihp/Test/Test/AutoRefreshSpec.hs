{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module: Test.AutoRefreshSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.AutoRefreshSpec where

import Test.Hspec
import IHP.Prelude
import IHP.AutoRefresh.Types
import qualified Data.Aeson as Aeson

tests :: Spec
tests = do
    describe "AutoRefresh change set" do
        it "stores row json and allows field access" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a001"
            let row = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId, "name" Aeson..= ("Riley" :: Text)]
            let payload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadRowId = Aeson.toJSON userId }
            let changeSet = insertRowChange "users" payload row mempty
            let [change] = changesForTable "users" changeSet
            change.table `shouldBe` "users"
            rowField @"userId" change `shouldBe` Just userId
            rowFieldByColumnName "user_id" row `shouldBe` Just userId
            rowsForTable "users" changeSet `shouldBe` [row]

        it "routes changes to the matching table slot" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a002"
            let projectId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a003"
            let userRow = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId]
            let projectRow = Aeson.object ["id" Aeson..= projectId, "user_id" Aeson..= userId]
            let userPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadRowId = Aeson.toJSON userId }
            let projectPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadRowId = Aeson.toJSON projectId }
            let changeSet =
                    mempty
                        |> insertRowChange "projects" projectPayload projectRow
                        |> insertRowChange "users" userPayload userRow
            length (changesForTable "projects" changeSet) `shouldBe` 1
            length (changesForTable "users" changeSet) `shouldBe` 1

        it "detects table changes" do
            let row = Aeson.object ["id" Aeson..= (1 :: Int)]
            let payload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadRowId = Aeson.toJSON (1 :: Int) }
            let changeSet = insertRowChange "users" payload row mempty
            anyChangeOnTable "users" changeSet `shouldBe` True
            anyChangeOnTable "projects" changeSet `shouldBe` False

        it "checks fields across all tables without table filtering" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a004"
            let userRow = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId]
            let projectRow = Aeson.object ["id" Aeson..= ("p-1" :: Text), "user_id" Aeson..= userId]
            let userPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadRowId = Aeson.toJSON userId }
            let projectPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadRowId = Aeson.toJSON ("p-1" :: Text) }
            let changeSet =
                    mempty
                        |> insertRowChange "users" userPayload userRow
                        |> insertRowChange "projects" projectPayload projectRow
            anyChangeWithField @"userId" userId changeSet `shouldBe` True
