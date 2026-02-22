-- hsx
module IHP.HSX.FileLevelSpec where

import Test.Hspec
import Prelude
import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Renderer.Text as Blaze
import qualified Data.Text.Lazy as TL

tests :: SpecWith ()
tests = describe "HSX file-level mode" do
    it "should render basic static HTML without quasiquotes" do
        let view =
                <div class="hero">
                    <h1>Hello</h1>
                    <p>Welcome</p>
                </div>
        Blaze.renderHtml view `shouldBe` TL.pack "<div class=\"hero\"><h1>Hello</h1><p>Welcome</p></div>"

    it "should handle attributes and void tags" do
        let view =
                <div id="root">
                    <br/>
                    <input disabled />
                </div>
        Blaze.renderHtml view `shouldBe` TL.pack "<div id=\"root\"><br><input disabled=\"disabled\"></div>"

    it "should allow sibling root nodes without fragments" do
        let stylesheets =
                <link rel="stylesheet" href="/a.css"/>
                <link rel="stylesheet" href="/b.css"/>
        Blaze.renderHtml stylesheets `shouldBe` TL.pack "<link rel=\"stylesheet\" href=\"/a.css\"><link rel=\"stylesheet\" href=\"/b.css\">"

    it "should allow nested HSX inside splices" do
        let isAdmin = True
        let view =
                <div>
                    {if isAdmin
                        then <span>Admin</span>
                        else <span>User</span>
                    }
                </div>
        Blaze.renderHtml view `shouldBe` TL.pack "<div><span>Admin</span></div>"

    it "should handle inline conditionals on a single line" do
        let isAdmin = False
        let view = <div>{if isAdmin then <span>Admin</span> else <span>User</span>}</div>
        Blaze.renderHtml view `shouldBe` TL.pack "<div><span>User</span></div>"

    it "should allow hsx after an in keyword" do
        let view =
                let isAdmin = True in <div>{if isAdmin then <span>Admin</span> else <span>User</span>}</div>
        Blaze.renderHtml view `shouldBe` TL.pack "<div><span>Admin</span></div>"

    it "should allow case expressions inside splices" do
        let role = "admin" :: String
        let view =
                <div>
                    {case role of
                        "admin" -> <span>Admin</span>
                        _ -> <span>User</span>
                    }
                </div>
        Blaze.renderHtml view `shouldBe` TL.pack "<div><span>Admin</span></div>"

    it "should allow let expressions inside splices" do
        let view =
                <div>
                    {let isAdmin = True in if isAdmin then <span>Admin</span> else <span>User</span>}
                </div>
        Blaze.renderHtml view `shouldBe` TL.pack "<div><span>Admin</span></div>"
