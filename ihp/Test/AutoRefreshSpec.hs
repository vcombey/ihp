module AutoRefreshSpec where

import IHP.Prelude
import Test.Hspec
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as List
import qualified Database.PostgreSQL.Simple.Types as PG
import IHP.AutoRefresh (notificationTrigger, channelName)

spec :: Spec
spec = describe "AutoRefresh triggers" do
    it "creates row-level triggers with id payload" do
        let table = "users" :: ByteString
        let sql = notificationTrigger table
        let sqlStr = case sql of PG.Query q -> B8.unpack q
        sqlStr `shouldSatisfy` (contains "FOR EACH ROW")
        sqlStr `shouldSatisfy` (contains "NEW.id::text")
        sqlStr `shouldSatisfy` (contains "OLD.id::text")

contains :: String -> String -> Bool
contains = List.isInfixOf
