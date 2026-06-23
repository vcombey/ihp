{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module IHP.RequestVault
( -- * Vault infrastructure (re-exported from Helper)
  module IHP.RequestVault.Helper
  -- * ModelContext (re-exported from ModelContext)
, module IHP.RequestVault.ModelContext
  -- * FrameworkConfig
, frameworkConfigVaultKey
, frameworkConfigMiddleware
, requestFrameworkConfig
  -- * Current application
, applicationContextVaultKey
, insertApplicationContext
, requestApplication
  -- * PGListener
, pgListenerVaultKey
, pgListenerMiddleware
, requestPGListener
) where

import IHP.Prelude
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vault.Lazy as Vault
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import IHP.FrameworkConfig
import IHP.PGListener
import IHP.RequestVault.Helper
import IHP.RequestVault.ModelContext

-- request.frameworkConfig
frameworkConfigVaultKey :: Vault.Key FrameworkConfig
frameworkConfigVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE frameworkConfigVaultKey #-}

{-# INLINE frameworkConfigMiddleware #-}
frameworkConfigMiddleware :: FrameworkConfig -> Middleware
frameworkConfigMiddleware = insertVaultMiddleware frameworkConfigVaultKey

{-# INLINE requestFrameworkConfig #-}
requestFrameworkConfig :: Request -> FrameworkConfig
requestFrameworkConfig = lookupRequestVault frameworkConfigVaultKey

-- request.pgListener
pgListenerVaultKey :: Vault.Key PGListener
pgListenerVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE pgListenerVaultKey #-}

{-# INLINE pgListenerMiddleware #-}
pgListenerMiddleware :: PGListener -> Middleware
pgListenerMiddleware = insertVaultMiddleware pgListenerVaultKey

{-# INLINE requestPGListener #-}
requestPGListener :: Request -> PGListener
requestPGListener = lookupRequestVault pgListenerVaultKey

-- Field access helpers
instance HasField "frameworkConfig" Request FrameworkConfig where
    {-# INLINE getField #-}
    getField request = requestFrameworkConfig request

-- request.application
applicationContextVaultKey :: Vault.Key Dynamic
applicationContextVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE applicationContextVaultKey #-}

{-# INLINE insertApplicationContext #-}
insertApplicationContext :: Typeable application => application -> Request -> Request
insertApplicationContext application request =
    request { vault = Vault.insert applicationContextVaultKey (toDyn application) request.vault }

{-# INLINE requestApplication #-}
requestApplication :: forall application. Typeable application => Request -> application
requestApplication request =
    case Vault.lookup applicationContextVaultKey request.vault >>= fromDynamic of
        Just application -> application
        Nothing -> error $ "requestApplication: Could not find " <> show (typeRep (Proxy @application)) <> " in request.vault. Did you forget to initialize the controller context?"

instance HasField "pgListener" Request PGListener where
    {-# INLINE getField #-}
    getField request = requestPGListener request
