{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
Module: IHP.AutoRefresh.ChangeSet
Description: Helpers for fine-grained Auto Refresh
-}
module IHP.AutoRefresh.ChangeSet
    ( AutoRefreshOperation (..)
    , AutoRefreshRowChange (..)
    , AutoRefreshChangeSet (..)
    , changesForTable
    , anyChangeOnTable
    , anyChangeWithField
    , rowField
    , rowFieldNew
    , rowFieldOld
    ) where

import IHP.AutoRefresh.Types
