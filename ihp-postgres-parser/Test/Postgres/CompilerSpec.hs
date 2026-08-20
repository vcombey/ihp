{-|
Module: Postgres.CompilerSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Postgres.CompilerSpec where

import Prelude
import Test.Hspec
import IHP.Postgres.Compiler (compileSql)
import IHP.Postgres.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.Conversions (cs)
import qualified Text.Megaparsec as Megaparsec
import IHP.Postgres.Parser (parseDDL)

spec :: Spec
spec = do
    describe "The Schema.sql Compiler" do
        it "should compile an empty CREATE TABLE statement" do
            compileSql [StatementCreateTable (table "users")] `shouldBe` "CREATE TABLE users (\n\n);\n"

        it "should compile a CREATE EXTENSION for the UUID extension" do
            compileSql [CreateExtension { name = "uuid-ossp", ifNotExists = True, extensionOptions = [] }] `shouldBe` "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n"

        it "should compile a line comment" do
            compileSql [Comment { content = " Comment value" }] `shouldBe` "-- Comment value\n"

        it "should compile a empty line comments" do
            compileSql [Comment { content = "" }, Comment { content = "" }] `shouldBe` "--\n--\n"

        it "should compile a CREATE TABLE with columns" do
            let sql = "CREATE TABLE users (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    firstname TEXT NOT NULL,\n    lastname TEXT NOT NULL,\n    password_hash TEXT NOT NULL,\n    email TEXT NOT NULL,\n    company_id UUID NOT NULL,\n    picture_url TEXT,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"
            let statement = StatementCreateTable (table "users")
                    { columns = [
                        (col "id" PUUID) { defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                        , (col "firstname" PText) { notNull = True }
                        , (col "lastname" PText) { notNull = True }
                        , (col "password_hash" PText) { notNull = True }
                        , (col "email" PText) { notNull = True }
                        , (col "company_id" PUUID) { notNull = True }
                        , col "picture_url" PText
                        , (col "created_at" PTimestampWithTimezone) { defaultValue = Just (CallExpression "NOW" []), notNull = True }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE with quoted identifiers" do
            compileSql [StatementCreateTable (table "quoted name")] `shouldBe` "CREATE TABLE \"quoted name\" (\n\n);\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE CASCADE" do
            let statement = AddConstraint
                    { tableName = "users"
                    , constraint = ForeignKeyConstraint
                        { name = Just "users_ref_company_id"
                        , columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = Just "id"
                        , onDelete = Just Cascade
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE CASCADE;\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = Just "check_title_length"
                        , checkExpression = NotEqExpression (VarExpression "title") (TextExpression "")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (title <> '');\n"

        -- See https://github.com/digitallyinduced/ihp/issues/2613: CHECK with ANY(ARRAY[...])
        -- is what pg_dump emits for IN constraints, so the compiler must round-trip it.
        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK with ANY(ARRAY[...])" do
            let statement = AddConstraint
                    { tableName = "foo"
                    , constraint = CheckConstraint
                        { name = Just "foo_kind_valid"
                        , checkExpression =
                            EqExpression
                                (VarExpression "kind")
                                (CallExpression "ANY"
                                    [ ArrayLiteralExpression
                                        [ TypeCastExpression (TextExpression "a") PText
                                        , TypeCastExpression (TextExpression "b") PText
                                        ]
                                    ])
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE foo ADD CONSTRAINT foo_kind_valid CHECK (kind = ANY(ARRAY['a'::TEXT, 'b'::TEXT]));\n"

        it "should compile a CREATE TYPE .. AS ENUM" do
            let sql = "CREATE TYPE colors AS ENUM ('yellow', 'red', 'blue');\n"
            let statement = CreateEnumType
                    { name = "colors"
                    , values = ["yellow", "red", "blue"]
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with a serial id" do
            let sql = "CREATE TABLE orders (\n    id SERIAL PRIMARY KEY NOT NULL\n);\n"
            let statement = StatementCreateTable (table "orders")
                    { columns = [ (col "id" PSerial) { notNull = True } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE INDEX statement" do
            let sql = "CREATE INDEX users_index ON users (user_name);\n"
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE UNIQUE INDEX statement" do
            let sql = "CREATE UNIQUE INDEX users_index ON users (user_name);\n"
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE UNIQUE INDEX with NULLS NOT DISTINCT" do
            let sql = "CREATE UNIQUE INDEX users_index ON users (user_name) NULLS NOT DISTINCT;\n"
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = False
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile pgvector column types with dimensions" do
            let sql = "ALTER TABLE knowledge_chunks ADD COLUMN embedding VECTOR(1536) DEFAULT NULL;\n"
            let statement = AddColumn
                    { tableName = "knowledge_chunks"
                    , column = (col "embedding" (PCustomType "VECTOR(1536)")) { defaultValue = Just (VarExpression "NULL") }
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile pgvector HNSW indexes with operator classes" do
            let sql = "CREATE INDEX knowledge_chunks_embedding_hnsw_idx ON knowledge_chunks USING HNSW (embedding vector_cosine_ops) WHERE embedding IS NOT NULL;\n"
            let statement = CreateIndex
                    { indexName = "knowledge_chunks_embedding_hnsw_idx"
                    , unique = False
                    , tableName = "knowledge_chunks"
                    , columns = [IndexColumn { column = VarExpression "embedding", columnOperatorClass = Just "vector_cosine_ops", columnOrder = [] }]
                    , whereClause = Just (IsExpression (VarExpression "embedding") (NotExpression (VarExpression "NULL")))
                    , indexType = Just Hnsw
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile pgvector IVFFLAT indexes with operator classes" do
            let sql = "CREATE INDEX knowledge_chunks_embedding_ivfflat_idx ON knowledge_chunks USING IVFFLAT (embedding vector_l2_ops);\n"
            let statement = CreateIndex
                    { indexName = "knowledge_chunks_embedding_ivfflat_idx"
                    , unique = False
                    , tableName = "knowledge_chunks"
                    , columns = [IndexColumn { column = VarExpression "embedding", columnOperatorClass = Just "vector_l2_ops", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Ivfflat
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile additional PostgreSQL index methods" do
            let compileMethod indexType = compileSql [CreateIndex
                    { indexName = "users_email_idx"
                    , unique = False
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "email")]
                    , whereClause = Nothing
                    , indexType = Just indexType
                    , nullsDistinct = True
                    }]
            compileMethod Hash `shouldBe` "CREATE INDEX users_email_idx ON users USING HASH (email);\n"
            compileMethod Spgist `shouldBe` "CREATE INDEX users_email_idx ON users USING SPGIST (email);\n"
            compileMethod Brin `shouldBe` "CREATE INDEX users_email_idx ON users USING BRIN (email);\n"

        it "should quote index operator class identifiers" do
            let sql = "CREATE INDEX knowledge_chunks_embedding_idx ON knowledge_chunks USING HNSW (embedding \"VectorOps\");\n"
            let statement = CreateIndex
                    { indexName = "knowledge_chunks_embedding_idx"
                    , unique = False
                    , tableName = "knowledge_chunks"
                    , columns = [IndexColumn { column = VarExpression "embedding", columnOperatorClass = Just "VectorOps", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Hnsw
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE FUNCTION with SET options" do
            let sql = "CREATE OR REPLACE FUNCTION sync_access() RETURNS TRIGGER SECURITY DEFINER SET search_path = public, private, pg_temp AS $$BEGIN\n    RETURN NEW;\nEND;$$ language plpgsql;\n"
            let statement = CreateFunction
                    { functionName = "sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = True
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, private, pg_temp"
                            }
                        ]
                    }
            compileSql [statement] `shouldBe` sql

        it "should round-trip a schema-qualified CREATE FUNCTION" do
            -- parse -> compile -> parse must preserve a non-public schema like `private.`
            let statement = CreateFunction
                    { functionName = "private.sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = True
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, private, pg_temp"
                            }
                        ]
                    }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should round-trip a schema-qualified DROP FUNCTION" do
            -- Guards against the CREATE/DROP asymmetry: both must accept `private.` names
            let statement = DropFunction { functionName = "private.sync_access" }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should compile a CREATE INDEX with VARIADIC function arguments" do
            let sql = "CREATE INDEX agent_runs_ingest_gmail_message_latest_idx ON agent_runs USING BTREE (organization_id, jsonb_extract_path_text(input, VARIADIC ARRAY['gmailMessageId'::TEXT]), COALESCE(completed_at, last_event_at, started_at, created_at) DESC, id DESC) WHERE type = ('ingest'::agent_run_type) AND jsonb_extract_path_text(input, VARIADIC ARRAY['source'::TEXT]) = ('gmail_email_ingest'::TEXT);\n"
            let statement = CreateIndex
                    { indexName = "agent_runs_ingest_gmail_message_latest_idx"
                    , unique = False
                    , tableName = "agent_runs"
                    , columns =
                            [ indexCol (VarExpression "organization_id")
                            , indexCol (CallExpression "jsonb_extract_path_text"
                                [ VarExpression "input"
                                , VariadicExpression (ArrayLiteralExpression [TypeCastExpression (TextExpression "gmailMessageId") PText])
                                ])
                            , IndexColumn
                                { column = CallExpression "COALESCE"
                                    [ VarExpression "completed_at"
                                    , VarExpression "last_event_at"
                                    , VarExpression "started_at"
                                    , VarExpression "created_at"
                                    ]
                                , columnOperatorClass = Nothing
                                , columnOrder = [Desc]
                                }
                            , IndexColumn { column = VarExpression "id", columnOperatorClass = Nothing, columnOrder = [Desc] }
                            ]
                    , whereClause = Just
                        (AndExpression
                            (EqExpression
                                (VarExpression "type")
                                (TypeCastExpression (TextExpression "ingest") (PCustomType "agent_run_type")))
                            (EqExpression
                                (CallExpression "jsonb_extract_path_text"
                                    [ VarExpression "input"
                                    , VariadicExpression (ArrayLiteralExpression [TypeCastExpression (TextExpression "source") PText])
                                    ])
                                (TypeCastExpression (TextExpression "gmail_email_ingest") PText)))
                    , indexType = Just Btree
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile 'ENABLE ROW LEVEL SECURITY' statements" do
            let sql = "ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;\n"
            let statements = [EnableRowLevelSecurity { tableName = "tasks" }]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE POLICY' statements" do
            let sql = "CREATE POLICY \"Users can manage their tasks\" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());\n"
            let p = (policy "Users can manage their tasks" "tasks")
                    { using = Just (
                        EqExpression
                            (VarExpression "user_id")
                            (CallExpression "ihp_user_id" [])
                        )
                    , check = Just (
                        EqExpression
                            (VarExpression "user_id")
                            (CallExpression "ihp_user_id" [])
                        )
                    }
            compileSql [p] `shouldBe` sql

        it "should compile 'DROP TABLE ..' statements" do
            let sql = "DROP TABLE tasks;\n"
            let statements = [ DropTable { tableName = "tasks" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE SEQUENCE ..' statements" do
            let sql = "CREATE SEQUENCE a;\n"
            let statements = [ CreateSequence { name = "a", sequenceOptions = [] } ]
            compileSql statements `shouldBe` sql

        it "should compile 'DROP TYPE ..;' statements" do
            let sql = "DROP TYPE colors;\n"
            let statements = [ DropEnumType { name = "colors" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'BEGIN;' statements" do
            let sql = "BEGIN;\n"
            let statements = [ Begin ]
            compileSql statements `shouldBe` sql

        it "should compile 'COMMIT;' statements" do
            let sql = "COMMIT;\n"
            let statements = [ Commit ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE TABLE .. INHERITS (..)' statements" do
            let sql = "CREATE TABLE post_revisions (\n    revision_content TEXT NOT NULL\n) INHERITS (posts);\n"
            let statements = [
                        StatementCreateTable (table "post_revisions")
                            { columns = [(col "revision_content" PText) { notNull = True }]
                            , inherits = Just "posts"
                            }
                        ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE UNLOGGED TABLE' statements" do
            let sql = "CREATE UNLOGGED TABLE pg_large_notifications (\n\n);\n"
            let statements = [
                        StatementCreateTable (table "pg_large_notifications")
                            { unlogged = True, inherits = Nothing
                            }
                        ]
            compileSql statements `shouldBe` sql

        describe "round trips the SQL a pg_dump contains" do
            let roundTrip sql = compileSql [parseSql sql] `shouldBe` (sql <> "\n")

            it "keeps a single column primary key added by ALTER TABLE" do
                roundTrip "ALTER TABLE ai_chat_accesses ADD CONSTRAINT ai_chat_accesses_pkey PRIMARY KEY(chat_id);"

            it "keeps a composite primary key added by ALTER TABLE" do
                roundTrip "ALTER TABLE memberships ADD CONSTRAINT memberships_pkey PRIMARY KEY(user_id, team_id);"

            it "adds an unnamed constraint without inventing a name" do
                roundTrip "ALTER TABLE users ADD CHECK (age > 0);"

            it "keeps the name of a constraint declared inside CREATE TABLE" do
                roundTrip "CREATE TABLE users (\n    age INT,\n    CONSTRAINT users_age_check CHECK (age > 0)\n);"

            it "keeps PostgreSQL 18 named NOT NULL constraints" do
                roundTrip "CREATE TABLE users (\n    email TEXT CONSTRAINT users_email_not_null NOT NULL\n);"

            it "keeps sequence options" do
                roundTrip "CREATE SEQUENCE property_ids AS BIGINT START WITH 1000000000000 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;"

            it "keeps the scale of numeric literals" do
                roundTrip "CREATE TABLE fees (\n    vat NUMERIC(7,4) DEFAULT 20.0000 NOT NULL\n);"

            it "keeps PostGIS geometry modifiers" do
                roundTrip "CREATE TABLE locations (\n    geom GEOMETRY(Point, 4326)\n);"

            it "keeps apostrophes in string literals" do
                roundTrip "ALTER TABLE fees ADD CHECK (label <> 'taxe d''enlevement');"

            -- The operands come back parenthesised, which is the compiler's canonical
            -- form for AND and parses to the same expression.
            it "keeps a CHECK constraint combining comparisons with AND" do
                compileSql [parseSql "ALTER TABLE t ADD CONSTRAINT t_positive CHECK (a > 0 AND b > 0);"]
                    `shouldBe` "ALTER TABLE t ADD CONSTRAINT t_positive CHECK ((a > 0) AND (b > 0));\n"

            it "keeps boolean IS expressions grouped inside equality" do
                roundTrip "ALTER TABLE t ADD CONSTRAINT t_pair CHECK ((a IS NULL) = (b IS NULL));"

            it "keeps an operator it has no constructor for" do
                roundTrip "ALTER TABLE t ADD CONSTRAINT t_code CHECK (code ~ '^[A-Z]{3}$');"

            it "keeps POSITION's SQL-standard IN syntax" do
                roundTrip "ALTER TABLE users ADD CONSTRAINT users_email_at CHECK (POSITION('@' IN email) > 1);"

            it "keeps a referential action restricted to some columns" do
                roundTrip "ALTER TABLE t ADD CONSTRAINT t_fk FOREIGN KEY (a, b) REFERENCES o (a, b) ON DELETE SET NULL (b);"

            it "keeps GRANT and REVOKE statements" do
                roundTrip "REVOKE ALL ON FUNCTION public.touch_updated_at() FROM PUBLIC;"

            it "keeps extension schema, version and cascade options" do
                roundTrip "CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA public VERSION '3.4.2' CASCADE;"

            it "keeps SQL COMMENT statements executable" do
                compileSql [parseSql "COMMENT ON TABLE users IS 'Owner''s records';"]
                    `shouldBe` "COMMENT ON TABLE users IS 'Owner''s records';\n"

            it "keeps non-public table schemas instead of moving objects to public" do
                roundTrip "CREATE TABLE private.tokens (id UUID);"
                roundTrip "ALTER TABLE private.tokens ENABLE ROW LEVEL SECURITY;"

            it "keeps unmodelled SQL containing semicolons" do
                roundTrip "CREATE PROCEDURE p() LANGUAGE plpgsql AS $$BEGIN PERFORM 1; END$$;"

            it "keeps function attributes" do
                roundTrip "CREATE FUNCTION f() RETURNS BOOLEAN IMMUTABLE PARALLEL SAFE COST 5 AS $$SELECT true;$$ language sql;"

            it "keeps trigger WHEN conditions" do
                roundTrip "CREATE TRIGGER t BEFORE UPDATE ON users FOR EACH ROW WHEN (OLD.email <> NEW.email) EXECUTE FUNCTION changed();"
                roundTrip "CREATE CONSTRAINT TRIGGER ct AFTER UPDATE ON users DEFERRABLE FOR EACH ROW WHEN (OLD.email <> NEW.email) EXECUTE FUNCTION changed();"

            it "picks a dollar quote the function body does not contain" do
                compileSql [(function "f") { functionBody = " BEGIN RETURN $$x$$; END; ", language = "plpgsql" }]
                    `shouldBe` "CREATE FUNCTION f() RETURNS TRIGGER AS $_$ BEGIN RETURN $$x$$; END; $_$ language plpgsql;\n"

parseSql :: Text -> Statement
parseSql sql =
    case Megaparsec.runParser parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError)
            Right [statement] -> statement
            Right statements -> error $ "Expected single statement but got: " <> show (length statements)
