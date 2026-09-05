-- | Explicit compilation transactions for the optional development object cache.
-- The helper owns kernel locks; closing its input releases them even on failure.
module IHP.IDE.DevCache (DevCache, withDevCache, finishDevCache, withDevCacheCompilation) where

import Prelude
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified System.Environment as Env
import System.IO (Handle, hGetLine, hPutStrLn, hFlush)
import qualified System.Process as Process

data DevCache = DevCache Handle Handle

request :: DevCache -> String -> IO ()
request (DevCache input output) event = do
    hPutStrLn input event
    hFlush input
    response <- hGetLine output
    if response == "ok" then pure () else fail "IHP development cache protocol failure"

-- | Disabled unless the app's Nix module supplies the packaged helper executable.
withDevCache :: String -> Bool -> [String] -> ([String] -> Maybe DevCache -> IO a) -> IO a
withDevCache role wrapWithDirenv arguments action = do
    helper <- Env.lookupEnv "IHP_DEV_CACHE_HELPER"
    case helper of
        Nothing -> action arguments Nothing
        Just executable -> do
            -- Fingerprint the same environment in which the compiler will run.
            let helperArguments = [role, LazyByteString.unpack (Aeson.encode arguments)]
            let baseParams = if wrapWithDirenv
                    then Process.proc "direnv" (["exec", ".", executable] <> helperArguments)
                    else Process.proc executable helperArguments
            let params = baseParams
                    { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe }
            Process.withCreateProcess params $ \input output _ _ -> case (input, output) of
                (Just cacheInput, Just cacheOutput) -> do
                    rawArguments <- hGetLine cacheOutput
                    configuredArguments <- either fail pure (Aeson.eitherDecode (LazyByteString.pack rawArguments))
                    let cache = DevCache cacheInput cacheOutput
                    request cache "begin"
                    action configuredArguments (Just cache)
                _ -> fail "IHP development cache pipes unavailable"

-- | Called after the IDE's existing compiler completion handshake, before app startup.
finishDevCache :: Maybe DevCache -> Either a b -> IO ()
finishDevCache Nothing _ = pure ()
finishDevCache (Just cache) result = request cache (either (const "failure") (const "success") result)

-- | Hold a build slot for a reload only, not while the application serves requests.
withDevCacheCompilation :: Maybe DevCache -> IO (Either a b) -> IO (Either a b)
withDevCacheCompilation Nothing action = action
withDevCacheCompilation cache@(Just state) action = do
    request state "begin"
    result <- action `Exception.onException` request state "failure"
    finishDevCache cache result
    pure result
