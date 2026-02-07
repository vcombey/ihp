{-|
Module: IHP.LocalFirst
Description: Local-first controller action marker and context helpers
Copyright: (c) digitally induced GmbH, 2026
-}
module IHP.LocalFirst where

import IHP.Prelude
import IHP.Controller.Context
import IHP.ControllerSupport
import IHP.ModelSupport
import IHP.Router.UrlGenerator (HasPath (..))
import IHP.LocalFirst.Types
import IHP.LocalFirst.Safety
import IHP.LocalFirst.CodeGen
import qualified Data.UUID.V4 as UUID
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef
import Network.Wai (Request)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Directory as Directory

-- | Initializes local-first context for a request.
--
-- Add this to your @initContext@ together with @initAutoRefresh@:
--
-- > initContext = do
-- >     setLayout defaultLayout
-- >     initAutoRefresh
-- >     initLocalFirst
initLocalFirst :: (?context :: ControllerContext) => IO ()
initLocalFirst = do
    putContext LocalFirstDisabled
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
    localState <- fromMaybe LocalFirstDisabled <$> maybeFromContext @LocalFirstState
    case localState of
        LocalFirstDisabled -> do
            localRouteId <- UUID.nextRandom
            let localRoutePath = LocalRoutePath (pathTo ?theAction)
            putContext LocalFirstEnabled
                { routeId = localRouteId
                , routePath = localRoutePath
                , options = localOptions
                }
            runAction
        LocalFirstEnabled {} -> runAction

isLocalFirstEnabled :: (?context :: ControllerContext) => Bool
isLocalFirstEnabled = case fromFrozenContext @LocalFirstState of
    LocalFirstEnabled {} -> True
    LocalFirstDisabled -> False

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
