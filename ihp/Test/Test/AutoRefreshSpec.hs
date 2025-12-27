{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module: Test.AutoRefreshSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.AutoRefreshSpec where

import Test.Hspec
import IHP.Prelude
import IHP.AutoRefresh.Types
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple.ToField as PG

tests :: Spec
tests = do
    describe "AutoRefresh change set" do
        it "keeps row json and extracts the primary key" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a001"
            let row = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId]
            let payload = AutoRefreshRowChangePayload { operation = AutoRefreshUpdate, row }
            let changeSet = insertChangeByTableName @'[User] "users" payload (emptyChangeSet @'[User])
            let [change] = changesFor @User changeSet
            change.rowId `shouldBe` Just (Id userId :: Id User)
            rowField @"userId" change `shouldBe` Just userId
            rowsFor @User changeSet `shouldBe` [row]

        it "skips row ids when the primary key column is missing" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a002"
            let row = Aeson.object ["user_id" Aeson..= userId]
            let payload = AutoRefreshRowChangePayload { operation = AutoRefreshUpdate, row }
            let changeSet = insertChangeByTableName @'[User] "users" payload (emptyChangeSet @'[User])
            rowIdsFor @User changeSet `shouldBe` []

        it "routes changes to the matching table slot" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a003"
            let projectId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a004"
            let userRow = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId]
            let projectRow = Aeson.object ["id" Aeson..= projectId, "user_id" Aeson..= userId]
            let userPayload = AutoRefreshRowChangePayload { operation = AutoRefreshInsert, row = userRow }
            let projectPayload = AutoRefreshRowChangePayload { operation = AutoRefreshUpdate, row = projectRow }
            let changeSet =
                    emptyChangeSet @'[User, Project]
                        |> insertChangeByTableName @'[User, Project] "projects" projectPayload
                        |> insertChangeByTableName @'[User, Project] "users" userPayload
            length (changesFor @Project changeSet) `shouldBe` 1
            length (changesFor @User changeSet) `shouldBe` 1

        it "extracts composite primary keys from row json" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a005"
            let projectId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a006"
            let row = Aeson.object ["user_id" Aeson..= userId, "project_id" Aeson..= projectId]
            let payload = AutoRefreshRowChangePayload { operation = AutoRefreshUpdate, row }
            let changeSet = insertChangeByTableName @'[Membership] "memberships" payload (emptyChangeSet @'[Membership])
            let membershipId = Id (userId, projectId) :: Id Membership
            hasRowId membershipId changeSet `shouldBe` True
            let [change] = changesFor @Membership changeSet
            rowField @"projectId" change `shouldBe` Just projectId

data User = User { id :: UUID, userId :: UUID }
type instance GetTableName User = "users"
type instance PrimaryKey "users" = UUID
instance Table User where
    columnNames = ["id", "user_id"]
    primaryKeyColumnNames = ["id"]
    primaryKeyConditionForId = PG.toField

data Project = Project { id :: UUID, userId :: UUID }
type instance GetTableName Project = "projects"
type instance PrimaryKey "projects" = UUID
instance Table Project where
    columnNames = ["id", "user_id"]
    primaryKeyColumnNames = ["id"]
    primaryKeyConditionForId = PG.toField

data Membership = Membership { userId :: UUID, projectId :: UUID }
type instance GetTableName Membership = "memberships"
type instance PrimaryKey "memberships" = (UUID, UUID)
instance Table Membership where
    columnNames = ["user_id", "project_id"]
    primaryKeyColumnNames = ["user_id", "project_id"]
    primaryKeyConditionForId (Id (userId, projectId)) =
        PG.Many [PG.Plain "(", PG.toField userId, PG.Plain ",", PG.toField projectId, PG.Plain ")"]
