{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.AutoRefresh.ChangeSet where

import IHP.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson

data AutoRefreshChange = AutoRefreshChange
    { table :: !Text
    , operation :: !Text
    , oldRow :: !(Maybe Aeson.Value)
    , newRow :: !(Maybe Aeson.Value)
    } deriving (Eq, Show)

newtype AutoRefreshChangeSet = AutoRefreshChangeSet { changes :: [AutoRefreshChange] }
    deriving (Eq, Show)

-- Missing rows mean unknown, never an empty change set. This includes legacy
-- notifications, oversized payloads, TRUNCATE and statement-level view changes.
decodeChangeSet :: Text -> ByteString -> Maybe AutoRefreshChangeSet
decodeChangeSet table payload = do
    value <- Aeson.decodeStrict' payload
    Aeson.parseMaybe (Aeson.withObject "AutoRefreshChange" \object -> do
        operation <- object Aeson..: "op"
        oldRow <- object Aeson..:? "old"
        newRow <- object Aeson..:? "new"
        let isRow (Just (Aeson.Object _)) = True
            isRow _ = False
        unless (case operation of
            "insert" -> isRow newRow
            "update" -> isRow oldRow && isRow newRow
            "delete" -> isRow oldRow
            _ -> False) (fail "Incomplete row change")
        pure (AutoRefreshChangeSet [AutoRefreshChange { table, operation, oldRow, newRow }])) value

changesForTable :: Text -> AutoRefreshChangeSet -> [AutoRefreshChange]
changesForTable name = filter (\change -> change.table == name) . (.changes)

anyChangeOnTable :: Text -> AutoRefreshChangeSet -> Bool
anyChangeOnTable name = not . null . changesForTable name

-- Check both sides of tenant/key transfers; unpublished columns are unknown.
anyChangeWithField :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => (value -> Bool) -> AutoRefreshChangeSet -> Bool
anyChangeWithField predicate set = any matches set.changes
  where
    matches change = any (maybe True predicate . rowFieldByColumnName column) (catMaybes [change.oldRow, change.newRow])
    column = fieldNameToColumnName (symbolToText @field)

rowFieldOld :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshChange -> Maybe value
rowFieldOld change = change.oldRow >>= rowFieldByColumnName (fieldNameToColumnName (symbolToText @field))

rowFieldNew :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshChange -> Maybe value
rowFieldNew change = change.newRow >>= rowFieldByColumnName (fieldNameToColumnName (symbolToText @field))

rowFieldByColumnName :: Aeson.FromJSON value => Text -> Aeson.Value -> Maybe value
rowFieldByColumnName name = \case
    Aeson.Object object -> KeyMap.lookup (Key.fromText name) object >>= Aeson.parseMaybe Aeson.parseJSON
    _ -> Nothing
