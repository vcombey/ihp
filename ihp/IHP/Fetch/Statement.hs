{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, DataKinds, TypeFamilies, PolyKinds, ConstraintKinds, TypeOperators, BangPatterns, FlexibleContexts #-}
{-|
Module: IHP.Fetch.Statement
Description: Prepared statements for fetch operations
Copyright: (c) digitally induced GmbH, 2025

Internal module containing hasql 'Statement' definitions shared by
'IHP.Fetch' and 'IHP.FetchPipelined'.
-}
module IHP.Fetch.Statement
( buildQueryListStatement
, buildQueryVectorStatement
, buildQueryMaybeStatement
, buildCountStatement
, buildExistsStatement
, selectModelColumns
) where

import Prelude
import IHP.ModelSupport (Table(..), GetModelByTableName)
import IHP.Hasql.FromRow (FromRowHasql(..))
import qualified Hasql.Statement as Hasql
import qualified Hasql.Decoders as Decoders
import IHP.QueryBuilder.Types (QueryBuilder(..), SQLQuery(..), qualifyAndJoinColumns)
import IHP.QueryBuilder.Compiler (buildQuery)
import IHP.QueryBuilder.HasqlCompiler (buildStatement, buildWrappedStatement)
import Data.Int (Int64)
import Data.Vector (Vector)

-- | Build a statement that fetches all rows matching a query builder.
buildQueryListStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () [model]
buildQueryListStatement !queryBuilder =
    buildStatement (buildQuery (selectModelColumns @model queryBuilder)) (Decoders.rowList (hasqlRowDecoder @model))

-- | Like 'buildQueryListStatement', but returns a 'Vector' instead of a list.
buildQueryVectorStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () (Vector model)
buildQueryVectorStatement !queryBuilder =
    buildStatement (buildQuery (selectModelColumns @model queryBuilder)) (Decoders.rowVector (hasqlRowDecoder @model))

-- | Build a statement that fetches at most one row (adds LIMIT 1).
buildQueryMaybeStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () (Maybe model)
buildQueryMaybeStatement !queryBuilder =
    buildStatement ((buildQuery (selectModelColumns @model queryBuilder)) { limitClause = Just 1 }) (Decoders.rowMaybe (hasqlRowDecoder @model))

-- | Replaces the deferred @table.*@ projection of a generated relation query
-- with the related model's explicit decoder order.
selectModelColumns :: forall model table. Table model => QueryBuilder table -> QueryBuilder table
selectModelColumns queryBuilder@(QueryBuilder sqlQuery@SQLQuery { columns = selectedColumns, selectFrom })
    | not (null selectedColumns) = queryBuilder
    | otherwise =
        let cols = columnNames @model
        in QueryBuilder sqlQuery
            { columns = cols
            , columnsSql = qualifyAndJoinColumns selectFrom cols
            }

-- | Build a @SELECT COUNT(*)@ statement wrapping a query builder.
buildCountStatement :: QueryBuilder table -> Hasql.Statement () Int64
buildCountStatement !queryBuilder =
    buildWrappedStatement "SELECT COUNT(*) FROM (" (buildQuery queryBuilder) ") AS _count_values" (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

-- | Build a @SELECT EXISTS@ statement wrapping a query builder.
buildExistsStatement :: QueryBuilder table -> Hasql.Statement () Bool
buildExistsStatement !queryBuilder =
    buildWrappedStatement "SELECT EXISTS (" (buildQuery queryBuilder) ") AS _exists_values" (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
