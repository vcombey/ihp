-- Database-free Cabal regression: cabal test ihp-typed-sql:schema-dependency-paths
module Main where

import Prelude
import Control.Exception (bracket_)
import Control.Monad (unless)
import qualified Data.Set as Set
import qualified Data.Text as Text
import IHP.TypedSql.CompileTimeDatabase (dependentSchemaFiles, dependentSchemaFilesForTables)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = withSystemTempDirectory "ihp-schema-paths" $ \temporary -> do
    base <- canonicalizePath temporary
    original <- getCurrentDirectory
    let framework = base </> "IHPSchema.sql"
    writeFile framework ""
    setEnv "IHP_TYPED_SQL_IHP_SCHEMA" framework
    let fixture directory = do
            createDirectoryIfMissing True (directory </> "Application")
            createDirectoryIfMissing True (directory </> "build/Generated/ActualTypes")
            writeFile (directory </> "Application/Schema.sql") "CREATE TABLE users (id UUID PRIMARY KEY);"
            writeFile (directory </> "build/Generated/TypedSqlSchemaDependencies") "users"
            writeFile (directory </> "build/Generated/ActualTypes/User.hs") "module Generated.ActualTypes.User where"
    let check directory = bracket_ (setCurrentDirectory directory) (setCurrentDirectory original) $ do
            fallback <- dependentSchemaFiles
            granular <- dependentSchemaFilesForTables (Just (Set.singleton (Text.pack "users")))
            missing <- dependentSchemaFilesForTables (Just (Set.singleton (Text.pack "missing")))
            unless (fallback == ["Application/Schema.sql", framework]) (fail (show fallback))
            unless (granular == ["build/Generated/TypedSqlSchemaDependencies", "build/Generated/ActualTypes/User.hs", framework]) (fail (show granular))
            unless (missing == fallback) (fail "missing generated tables must retain full schema dependency")
            pure granular
    let first = base </> "first"
    let second = base </> "second"
    fixture first
    fixture second
    firstPaths <- check first
    secondPaths <- check second
    unless (firstPaths == secondPaths) (fail "application dependency paths differ across worktrees")
    putStrLn "PASS: full-schema fallback, missing-table fallback, granular paths and relocation"
