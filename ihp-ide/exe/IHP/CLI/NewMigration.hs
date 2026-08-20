module Main where

import IHP.Prelude
import qualified System.Posix.Env.ByteString as Posix
import qualified System.Directory.OsPath as Directory
import IHP.IDE.ToolServer.Helper.Controller (openEditor)
import qualified IHP.IDE.CodeGen.MigrationGenerator as MigrationGenerator
import IHP.IDE.CodeGen.Controller (executePlan)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
    ensureIsInAppDirectory

    let doCreateMigration description = do
            (_, plan) <- MigrationGenerator.buildPlan description Nothing
            executePlan plan
            let paths = MigrationGenerator.migrationPathsFromPlan plan
            case paths of
                [path] -> putStrLn $ "Created migration: " <> path
                _ -> do
                    putStrLn "Created migrations:"
                    forM_ paths (putStrLn . ("- " <>))
            openEditor (MigrationGenerator.migrationPathFromPlan plan) 0 0
    
    args <- Posix.getArgs
    case headMay args of
        Just "--help" -> usage
        Just description -> doCreateMigration (cs description)
        Nothing -> doCreateMigration ""

usage :: IO ()
usage = putStrLn "Usage: new-migration [DESCRIPTION]"

ensureIsInAppDirectory :: IO ()
ensureIsInAppDirectory = do
    -- Multi-executable projects have no root Main.hs, but every IHP project has a
    -- Schema.sql: that is the file this generator actually works with.
    mainHsExists <- Directory.doesFileExist "Main.hs"
    schemaExists <- Directory.doesFileExist "Application/Schema.sql"
    unless (mainHsExists || schemaExists) (fail "You have to be in a project directory to run the generator")
