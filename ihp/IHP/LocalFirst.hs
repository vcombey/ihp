{-|
Module: IHP.LocalFirst
Description: Local-first controller action marker and context helpers
Copyright: (c) digitally induced GmbH, 2026
-}
module IHP.LocalFirst where

import IHP.Prelude
import IHP.Controller.Context
import IHP.ControllerSupport
import IHP.Router.UrlGenerator (HasPath (..))
import IHP.LocalFirst.Types
import IHP.LocalFirst.Safety
import IHP.LocalFirst.CodeGen
import qualified Data.UUID.V4 as UUID
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import qualified Data.TMap as TypeMap
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Directory as Directory

-- | Initializes local-first context for a request.
--
-- Add this to your @initContext@:
--
-- > initContext = do
-- >     setLayout defaultLayout
-- >     initLocalFirst
initLocalFirst :: (?context :: ControllerContext) => IO ()
initLocalFirst = do
    ensureLocalFirstArtifacts

-- | Marks a controller action as local-first.
--
-- Use this similar to @autoRefresh@:
--
-- > action ShowTodoAction = local do
-- >     todos <- query @Todo |> fetch
-- >     render ShowView { .. }
local ::
    ( ?theAction :: action
    , HasPath action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
local = localWith defaultLocalOptions

-- | Same as 'local', but allows custom local runtime policies.
localWith ::
    ( ?theAction :: action
    , HasPath action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => LocalOptions -> ((?modelContext :: ModelContext) => IO ()) -> IO ()
localWith localOptions runAction = do
    case Vault.lookup localFirstStateVaultKey ?request.vault of
        Just LocalFirstEnabled {} -> runAction
        _ -> do
            localRouteId <- UUID.nextRandom
            let localRoutePath = LocalRoutePath (pathTo ?theAction)
            let localState = LocalFirstEnabled
                    { routeId = localRouteId
                    , routePath = localRoutePath
                    , options = localOptions
                    }
            let newRequest = ?request { Wai.vault = Vault.insert localFirstStateVaultKey localState ?request.vault }
            let ?request = newRequest

            -- Keep the request in the controller context aligned with the updated vault state.
            let ControllerContext { customFieldsRef } = ?context
            modifyIORef' customFieldsRef (TypeMap.insert @Wai.Request newRequest)

            runAction

type LocalOptionModifier = LocalOptions -> LocalOptions

-- | Same as 'local', but allows configuring options via small composable modifiers.
--
-- Example:
--
-- > -- inside your action body:
-- > localWithOptions
-- >     [ withSyncTables ["todos"]
-- >     , withConflictPolicy (LocalConflictLastWriteWinsBy "updatedAt")
-- >     , withReconnectProbePath "/healthz"
-- >     ]
-- >     do
-- >         todos <- query @Todo |> fetch
-- >         render TodosView { .. }
localWithOptions ::
    ( ?theAction :: action
    , HasPath action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => [LocalOptionModifier] -> ((?modelContext :: ModelContext) => IO ()) -> IO ()
localWithOptions modifiers = localWith (applyLocalOptionModifiers modifiers)

-- | Applies option modifiers to 'defaultLocalOptions'.
applyLocalOptionModifiers :: [LocalOptionModifier] -> LocalOptions
applyLocalOptionModifiers modifiers = foldl' (|>) defaultLocalOptions modifiers

-- | Configures conflict resolution policy for server-to-local merges.
withConflictPolicy :: LocalConflictPolicy -> LocalOptionModifier
withConflictPolicy conflictPolicy localOptions = localOptions { conflictPolicy }

-- | Restricts which tables are mirrored from server subscriptions to local DB.
--
-- Empty list means all tables.
withSyncTables :: [Text] -> LocalOptionModifier
withSyncTables syncTables localOptions = localOptions { syncTables }

-- | Replaces the reconnect probe policy.
withReconnectPolicy :: LocalReconnectPolicy -> LocalOptionModifier
withReconnectPolicy reconnectPolicy localOptions = localOptions { reconnectPolicy }

-- | Sets the reconnect probe request path.
withReconnectProbePath :: Text -> LocalOptionModifier
withReconnectProbePath probePath localOptions =
    localOptions
        { reconnectPolicy =
            (reconnectPolicy localOptions)
                { probePath = Just probePath
                }
        }

-- | Sets reconnect probe timeout in milliseconds.
withReconnectProbeTimeoutMs :: Int -> LocalOptionModifier
withReconnectProbeTimeoutMs probeTimeoutMs localOptions =
    localOptions
        { reconnectPolicy =
            (reconnectPolicy localOptions)
                { probeTimeoutMs
                }
        }

-- | Sets reconnect probe interval in milliseconds.
withReconnectProbeIntervalMs :: Int -> LocalOptionModifier
withReconnectProbeIntervalMs probeIntervalMs localOptions =
    localOptions
        { reconnectPolicy =
            (reconnectPolicy localOptions)
                { probeIntervalMs
                }
        }

isLocalFirstEnabled :: (?context :: ControllerContext) => Bool
isLocalFirstEnabled = case Vault.lookup localFirstStateVaultKey ?context.request.vault of
    Just LocalFirstEnabled {} -> True
    _ -> False

-- | Scans all Haskell sources below the given project root and writes:
-- - Generated/LocalRoutes.hs
-- - local-routes.manifest.json
--
-- Returns all detected local-safety violations.
generateLocalFirstArtifacts :: FilePath -> IO [LocalSafetyViolation]
generateLocalFirstArtifacts = writeLocalRouteArtifacts

ensureLocalFirstArtifacts :: IO ()
ensureLocalFirstArtifacts = do
    shouldGenerate <- IORef.atomicModifyIORef' localFirstArtifactsGenerated \alreadyGenerated ->
        if alreadyGenerated
            then (True, False)
            else (True, True)
    when shouldGenerate do
        projectRoot <- Directory.getCurrentDirectory
        _ <- Exception.try (writeLocalRouteArtifacts projectRoot) :: IO (Either Exception.SomeException [LocalSafetyViolation])
        pure ()

localFirstArtifactsGenerated :: IORef.IORef Bool
localFirstArtifactsGenerated = unsafePerformIO (IORef.newIORef False)
{-# NOINLINE localFirstArtifactsGenerated #-}

localFirstStateVaultKey :: Vault.Key LocalFirstState
localFirstStateVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE localFirstStateVaultKey #-}
