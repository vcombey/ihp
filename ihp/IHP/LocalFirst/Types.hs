{-|
Module: IHP.LocalFirst.Types
Description: Types for local-first controller execution
Copyright: (c) digitally induced GmbH, 2026
-}
module IHP.LocalFirst.Types where

import IHP.Prelude

data LocalSyncPolicy
    = LocalSyncServerWins
    deriving (Eq, Show)

data LocalAuthPolicy
    = LocalAuthLastAuthenticatedUser
    deriving (Eq, Show)

data LocalSchemaPolicy
    = LocalSchemaWholeApp
    deriving (Eq, Show)

data LocalReconnectPolicy = LocalReconnectPolicy
    { probePath :: !(Maybe Text)
    , probeTimeoutMs :: !Int
    , probeIntervalMs :: !Int
    } deriving (Eq, Show)

defaultLocalReconnectPolicy :: LocalReconnectPolicy
defaultLocalReconnectPolicy = LocalReconnectPolicy
    { probePath = Nothing
    , probeTimeoutMs = 2000
    , probeIntervalMs = 15000
    }

data LocalOptions = LocalOptions
    { syncPolicy :: !LocalSyncPolicy
    , authPolicy :: !LocalAuthPolicy
    , schemaPolicy :: !LocalSchemaPolicy
    , syncTables :: ![Text]
    , reconnectPolicy :: !LocalReconnectPolicy
    } deriving (Eq, Show)

defaultLocalOptions :: LocalOptions
defaultLocalOptions = LocalOptions
    { syncPolicy = LocalSyncServerWins
    , authPolicy = LocalAuthLastAuthenticatedUser
    , schemaPolicy = LocalSchemaWholeApp
    , syncTables = []
    , reconnectPolicy = defaultLocalReconnectPolicy
    }

newtype LocalRoutePath = LocalRoutePath Text
    deriving (Eq, Show)

data LocalFirstState
    = LocalFirstDisabled
    | LocalFirstEnabled
        { routeId :: !UUID
        , routePath :: !LocalRoutePath
        , options :: !LocalOptions
        }
    deriving (Eq, Show)

localMetaProperty :: ByteString
localMetaProperty = "ihp-local-route"
