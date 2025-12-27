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
import IHP.ModelSupport
import Control.Concurrent.MVar (MVar)
import qualified IHP.PGListener as PGListener
import qualified Data.Aeson as Aeson
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Database.PostgreSQL.Simple.Types as PG

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
    , rowId :: !Aeson.Value
    } deriving (Eq, Show)

instance Aeson.FromJSON AutoRefreshRowChangePayload where
    parseJSON = Aeson.withObject "AutoRefreshRowChangePayload" \object ->
        AutoRefreshRowChangePayload
            <$> object Aeson..: "op"
            <*> object Aeson..: "id"

class AutoRefreshTableList (tables :: [Type]) where
    autoRefreshTableNames :: Set ByteString
    autoRefreshTableInfo :: [(ByteString, [ByteString])]
    emptyChangeSet :: AutoRefreshChangeSet tables
    insertChangeByTableName :: ByteString -> AutoRefreshRowChangePayload -> Aeson.Value -> AutoRefreshChangeSet tables -> AutoRefreshChangeSet tables
    fetchRowJsonByTableName :: (?modelContext :: ModelContext) => ByteString -> AutoRefreshRowChangePayload -> IO (Maybe Aeson.Value)

instance AutoRefreshTableList '[] where
    autoRefreshTableNames = mempty
    autoRefreshTableInfo = []
    emptyChangeSet = AutoRefreshChangeSetNil
    insertChangeByTableName _ _ _ changes = changes
    fetchRowJsonByTableName _ _ = pure Nothing

instance (Table model, Aeson.FromJSON (Id model), AutoRefreshTableList rest) => AutoRefreshTableList (model ': rest) where
    autoRefreshTableNames = Set.insert (tableNameByteString @model) (autoRefreshTableNames @rest)
    autoRefreshTableInfo = (tableNameByteString @model, primaryKeyColumnNames @model) : (autoRefreshTableInfo @rest)
    emptyChangeSet = AutoRefreshChangeSetCons mempty (emptyChangeSet @rest)
    insertChangeByTableName tableName payload row (AutoRefreshChangeSetCons change rest)
        | tableName == tableNameByteString @model = AutoRefreshChangeSetCons (insertRowChange payload row change) rest
        | otherwise = AutoRefreshChangeSetCons change (insertChangeByTableName @rest tableName payload row rest)
        where
            insertRowChange AutoRefreshRowChangePayload { operation, rowId } row (AutoRefreshTableChanges rows) =
                let parsedRowId = Aeson.parseMaybe Aeson.parseJSON rowId
                in AutoRefreshTableChanges (AutoRefreshRowChange { operation, row, rowId = parsedRowId } : rows)
    fetchRowJsonByTableName tableName payload
        | tableName == tableNameByteString @model = fetchRowJsonById payload
        | otherwise = fetchRowJsonByTableName @rest tableName payload
        where
            fetchRowJsonById AutoRefreshRowChangePayload { rowId } =
                case Aeson.parseMaybe Aeson.parseJSON rowId of
                    Just parsedRowId -> do
                        let query = "SELECT row_to_json(" <> tableNameByteString @model <> ") FROM " <> tableNameByteString @model <> " WHERE " <> primaryKeyConditionColumnSelector @model <> " = ?"
                        rows <- sqlQuery (PG.Query query) (PG.Only (primaryKeyConditionForId @model parsedRowId))
                        pure (headMay rows)
                    Nothing -> pure Nothing

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
        -- | Track delete events to force refresh without row data
        , pendingDelete :: !(IORef Bool)
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
