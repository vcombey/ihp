{-# LANGUAGE OverloadedStrings #-}

module IHP.HSX.FormatterSpec where

import Prelude
import qualified Data.Text as Text
import IHP.HSX.Format
import Test.Hspec

tests :: Spec
tests = do
    describe "hsxfmt" do
        it "formats a simple multiline hsx quote" do
            let input = Text.unlines
                    [ "module Example where"
                    , "view = [hsx|<div><span>{foo}</span></div>|]"
                    ]
            result <- formatSource (defaultFormatOptions "Example.hs") input
            result `shouldBe` Right (Text.unlines
                [ "module Example where"
                , "view = [hsx|"
                , "    <div>"
                , "        <span>{foo}</span>"
                , "    </div>"
                , "|]"
                ])

        it "keeps short single-node quotes inline" do
            let input = "renderOption x = [hsx|<option>{x}</option>|]"
            result <- formatSource (defaultFormatOptions "Example.hs") input
            result `shouldBe` Right "renderOption x = [hsx|<option>{x}</option>|]"

        it "ignores hsx markers in strings and comments" do
            let input = Text.unlines
                    [ "module Example where"
                    , "comment = \"[hsx|<div/>|]\""
                    , "-- [hsx|<span/>|]"
                    , "view = [hsx|<div>{foo}</div>|]"
                    ]
            result <- formatSource (defaultFormatOptions "Example.hs") input
            result `shouldBe` Right (Text.unlines
                [ "module Example where"
                , "comment = \"[hsx|<div/>|]\""
                , "-- [hsx|<span/>|]"
                , "view = [hsx|<div>{foo}</div>|]"
                ])

        it "formats multiline attributes and preserves script bodies as raw text" do
            let input = Text.unlines
                    [ "module Example where"
                    , "view = [hsx|<script data-foo={foo bar}>console.log(window.location.href);</script>|]"
                    ]
            result <- formatSource (defaultFormatOptions "Example.hs") input
            result `shouldBe` Right (Text.unlines
                [ "module Example where"
                , "view = [hsx|"
                , "    <script data-foo={foo bar}>"
                , "        console.log(window.location.href);"
                , "    </script>"
                , "|]"
                ])
