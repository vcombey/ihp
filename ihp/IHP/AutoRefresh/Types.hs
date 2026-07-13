{-|
Module: IHP.AutoRefresh.Types
Description: Types & Data Structures for IHP AutoRefresh
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh.Types where

import Control.Concurrent.MVar (MVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Dynamic (Dynamic)
import qualified Data.Map.Strict as Map
import qualified Data.UUID as UUID
import qualified IHP.PGListener as PGListener
import IHP.Prelude
import Network.Wai (Request, ResponseReceived)
import Wai.Request.Params.Middleware (Respond)

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

data AutoRefreshRowChangePayload = AutoRefreshRowChangePayload
    { payloadOperation :: !AutoRefreshOperation
    , payloadOldRow :: !(Maybe Aeson.Value)
    , payloadNewRow :: !(Maybe Aeson.Value)
    , payloadLargePayloadId :: !(Maybe UUID.UUID)
    }
    deriving (Eq, Show)

instance Aeson.FromJSON AutoRefreshRowChangePayload where
    parseJSON = Aeson.withObject "AutoRefreshRowChangePayload" \object ->
        AutoRefreshRowChangePayload
            <$> object Aeson..: "op"
            <*> object Aeson..:? "old"
            <*> object Aeson..:? "new"
            <*> do
                payloadId <- object Aeson..:? "payloadId"
                case payloadId of
                    Nothing -> pure Nothing
                    Just value -> Just <$> parseUUID value
      where
        parseUUID :: Text -> AesonTypes.Parser UUID.UUID
        parseUUID value = case UUID.fromText value of
            Just uuid -> pure uuid
            Nothing -> fail "Invalid UUID for payloadId"

data AutoRefreshState = AutoRefreshEnabled { sessionId :: !UUID }
data AutoRefreshSession = AutoRefreshSession
        { id :: !UUID
        -- | A callback to rerun an action within the given request and respond
        , renderView :: !(Request -> Respond -> IO ResponseReceived)
        -- | MVar that is filled whenever some table changed
        , event :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables :: !(Set Text)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing :: !UTCTime
        , trackedIds :: !(Map.Map Text (Set Text))
        , trackedConditions :: !(Map.Map Text [Maybe Dynamic])
        }

data AutoRefreshServer = AutoRefreshServer
        { subscriptions :: [PGListener.Subscription]
        , sessions :: ![AutoRefreshSession]
        , subscribedTables :: !(Set Text)
        , pgListener :: PGListener.PGListener
        }

newAutoRefreshServer :: PGListener.PGListener -> AutoRefreshServer
newAutoRefreshServer pgListener = AutoRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, pgListener }
