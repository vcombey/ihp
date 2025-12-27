{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module: IHP.AutoRefresh.Types
Description: Types & Data Structures for IHP AutoRefresh
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh.Types where

import IHP.Prelude
import IHP.Controller.RequestContext
import Control.Concurrent.MVar (MVar)
import qualified IHP.PGListener as PGListener
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector

data AutoRefreshOperation
    = AutoRefreshInsert
    | AutoRefreshUpdate
    | AutoRefreshDelete
    deriving (Eq, Show)

instance Aeson.FromJSON AutoRefreshOperation where
    parseJSON = Aeson.withText "AutoRefreshOperation" \operation ->
        case toLower operation of
            "insert" -> pure AutoRefreshInsert
            "update" -> pure AutoRefreshUpdate
            "delete" -> pure AutoRefreshDelete
            _ -> fail ("Unknown operation: " <> cs operation)

data AutoRefreshRowChange model = AutoRefreshRowChange
    { operation :: !AutoRefreshOperation
    , row :: !Aeson.Value
    , rowId :: !(Maybe (Id model))
    } deriving (Eq, Show)

newtype AutoRefreshTableChanges model = AutoRefreshTableChanges
    { rows :: [AutoRefreshRowChange model]
    } deriving (Eq, Show)

instance Semigroup (AutoRefreshTableChanges model) where
    AutoRefreshTableChanges left <> AutoRefreshTableChanges right = AutoRefreshTableChanges (left <> right)

instance Monoid (AutoRefreshTableChanges model) where
    mempty = AutoRefreshTableChanges []

data AutoRefreshChangeSet (tables :: [Type]) where
    AutoRefreshChangeSetNil :: AutoRefreshChangeSet '[]
    AutoRefreshChangeSetCons :: !(AutoRefreshTableChanges model) -> !(AutoRefreshChangeSet rest) -> AutoRefreshChangeSet (model ': rest)

class AutoRefreshChangeSetAppend (tables :: [Type]) where
    appendChangeSet :: AutoRefreshChangeSet tables -> AutoRefreshChangeSet tables -> AutoRefreshChangeSet tables

instance AutoRefreshChangeSetAppend '[] where
    appendChangeSet AutoRefreshChangeSetNil AutoRefreshChangeSetNil = AutoRefreshChangeSetNil

instance (AutoRefreshChangeSetAppend rest) => AutoRefreshChangeSetAppend (model ': rest) where
    appendChangeSet (AutoRefreshChangeSetCons left leftRest) (AutoRefreshChangeSetCons right rightRest) =
        AutoRefreshChangeSetCons (left <> right) (appendChangeSet leftRest rightRest)

instance AutoRefreshChangeSetAppend tables => Semigroup (AutoRefreshChangeSet tables) where
    (<>) = appendChangeSet

data AutoRefreshRowChangePayload = AutoRefreshRowChangePayload
    { operation :: !AutoRefreshOperation
    , row :: !Aeson.Value
    } deriving (Eq, Show)

instance Aeson.FromJSON AutoRefreshRowChangePayload where
    parseJSON = Aeson.withObject "AutoRefreshRowChangePayload" \object ->
        AutoRefreshRowChangePayload
            <$> object Aeson..: "op"
            <*> object Aeson..: "row"

class AutoRefreshTableList (tables :: [Type]) where
    autoRefreshTableNames :: Set ByteString
    autoRefreshTableInfo :: [(ByteString, [ByteString])]
    emptyChangeSet :: AutoRefreshChangeSet tables
    insertChangeByTableName :: ByteString -> AutoRefreshRowChangePayload -> AutoRefreshChangeSet tables -> AutoRefreshChangeSet tables

instance AutoRefreshTableList '[] where
    autoRefreshTableNames = mempty
    autoRefreshTableInfo = []
    emptyChangeSet = AutoRefreshChangeSetNil
    insertChangeByTableName _ _ changes = changes

instance (Table model, Aeson.FromJSON (Id model), AutoRefreshTableList rest) => AutoRefreshTableList (model ': rest) where
    autoRefreshTableNames = Set.insert (tableNameByteString @model) (autoRefreshTableNames @rest)
    autoRefreshTableInfo = (tableNameByteString @model, primaryKeyColumnNames @model) : (autoRefreshTableInfo @rest)
    emptyChangeSet = AutoRefreshChangeSetCons mempty (emptyChangeSet @rest)
    insertChangeByTableName tableName payload (AutoRefreshChangeSetCons change rest)
        | tableName == tableNameByteString @model = AutoRefreshChangeSetCons (insertRowChange payload change) rest
        | otherwise = AutoRefreshChangeSetCons change (insertChangeByTableName @rest tableName payload rest)
        where
            insertRowChange AutoRefreshRowChangePayload { operation, row } (AutoRefreshTableChanges rows) =
                let rowId = extractRowId @model row
                in AutoRefreshTableChanges (AutoRefreshRowChange { operation, row, rowId } : rows)

class AutoRefreshTableMember model (tables :: [Type]) where
    getTableChanges :: AutoRefreshChangeSet tables -> AutoRefreshTableChanges model

instance AutoRefreshTableMember model (model ': rest) where
    getTableChanges (AutoRefreshChangeSetCons changes _) = changes

instance (AutoRefreshTableMember model rest) => AutoRefreshTableMember model (other ': rest) where
    getTableChanges (AutoRefreshChangeSetCons _ rest) = getTableChanges @model rest

instance (AutoRefreshTableList tables, AutoRefreshChangeSetAppend tables) => Monoid (AutoRefreshChangeSet tables) where
    mempty = emptyChangeSet

changesFor :: AutoRefreshTableMember model tables => AutoRefreshChangeSet tables -> [AutoRefreshRowChange model]
changesFor = (.rows) . getTableChanges

rowIdsFor :: AutoRefreshTableMember model tables => AutoRefreshChangeSet tables -> [Id model]
rowIdsFor = mapMaybe (.rowId) . changesFor

anyChangeOnTable :: AutoRefreshTableMember model tables => AutoRefreshChangeSet tables -> Bool
anyChangeOnTable = not . null . changesFor

hasRowId :: (AutoRefreshTableMember model tables, Eq (Id model)) => Id model -> AutoRefreshChangeSet tables -> Bool
hasRowId rowId = any (\change -> change.rowId == Just rowId) . changesFor

rowsFor :: AutoRefreshTableMember model tables => AutoRefreshChangeSet tables -> [Aeson.Value]
rowsFor = map (.row) . changesFor

rowField :: forall field value model. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshRowChange model -> Maybe value
rowField change = rowFieldByColumnName (fieldNameToColumnName (symbolToText @field)) change.row

rowFieldByColumnName :: forall value. (Aeson.FromJSON value) => Text -> Aeson.Value -> Maybe value
rowFieldByColumnName columnName = \case
    Aeson.Object object -> do
        value <- AesonKeyMap.lookup (cs columnName) object
        Aeson.parseMaybe Aeson.parseJSON value
    _ -> Nothing

extractRowId :: forall model. (Table model, Aeson.FromJSON (Id model)) => Aeson.Value -> Maybe (Id model)
extractRowId = \case
    Aeson.Object object -> do
        let primaryKeys = primaryKeyColumnNames @model
        values <- traverse (\column -> AesonKeyMap.lookup (cs column) object) primaryKeys
        let idValue = case values of
                [value] -> value
                _ -> Aeson.Array (Vector.fromList values)
        Aeson.parseMaybe Aeson.parseJSON idValue
    _ -> Nothing

data AutoRefreshState = AutoRefreshDisabled | AutoRefreshEnabled { sessionId :: !UUID }
data AutoRefreshSession = AutoRefreshSession
        { id :: !UUID
        -- | A callback to rerun an action within a given request context
        , renderView :: !(RequestContext -> IO ())
        -- | MVar that is filled whenever some table changed
        , event :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables :: !(Set ByteString)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing :: !UTCTime
        }
    | forall tables. AutoRefreshTableList tables => AutoRefreshSessionWithChanges
        { id :: !UUID
        -- | A callback to rerun an action within a given request context
        , renderView :: !(RequestContext -> IO ())
        -- | MVar that is filled whenever some table changed
        , event :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables :: !(Set ByteString)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing :: !UTCTime
        -- | Pending changes coalesced since the last refresh
        , pendingChanges :: !(IORef (AutoRefreshChangeSet tables))
        -- | Decide if a refresh should run for the accumulated changes
        , shouldRefresh :: !(AutoRefreshChangeSet tables -> IO Bool)
        }

data AutoRefreshServer = AutoRefreshServer
        { subscriptions :: [PGListener.Subscription]
        , sessions :: ![AutoRefreshSession]
        , subscribedTables :: !(Set ByteString)
        , subscribedRowTables :: !(Set ByteString)
        , pgListener :: PGListener.PGListener
        }

newAutoRefreshServer :: PGListener.PGListener -> AutoRefreshServer
newAutoRefreshServer pgListener = AutoRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, subscribedRowTables = mempty, pgListener }
