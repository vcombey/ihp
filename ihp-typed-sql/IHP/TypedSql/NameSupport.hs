module IHP.TypedSql.NameSupport (tableNameToModelName) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.Conversions (cs)
import qualified Data.String.Conversions as CS
import Text.Countable (singularize)
import qualified Text.Inflections as Inflector

-- | Map a table name to its model name, e.g. @"users"@ to @"User"@.
-- Canonical definition, also re-exported from 'IHP.NameSupport' for IHP
-- apps (this package cannot depend on @ihp@, so the single implementation
-- lives here). Same singularization and camel-casing rules, including the
-- @"brain_waves"@ special case.
tableNameToModelName :: Text -> Text
tableNameToModelName "brain_waves" = "BrainWave"
tableNameToModelName tableName = do
    let singularizedTableName = cs (singularize tableName)
    if "_" `Text.isInfixOf` singularizedTableName
        then unwrapEither tableName $ Inflector.toCamelCased True $ singularizedTableName
        else ucfirst singularizedTableName
{-# INLINABLE tableNameToModelName #-}

unwrapEither :: Show err => Text -> Either err Text -> Text
unwrapEither _ (Right value) = value
unwrapEither input (Left value) = error (CS.cs ("IHP.TypedSql: " <> show value <> " (value to be transformed: " <> show input <> ")"))
{-# INLINABLE unwrapEither #-}

-- | Make a text's first character uppercase.
ucfirst :: Text -> Text
ucfirst = applyFirst Text.toUpper
{-# INLINABLE ucfirst #-}

applyFirst :: (Text -> Text) -> Text -> Text
applyFirst f text =
    let (first, rest) = Text.splitAt 1 text
    in (f first) <> rest
{-# INLINABLE applyFirst #-}
