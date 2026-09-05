module IDE.DevServerSpec where

import IHP.Prelude
import Test.Hspec
import qualified Data.ByteString.Char8 as ByteString
import IHP.IDE.Types (extractCrashMessage)
import IHP.IDE.GhciSupport (ghciLoadSucceeded)

ghciTests :: Spec
ghciTests = describe "GHCi compilation completion" do
    forM_ ["Ok, one module loaded.", "Ok, one module reloaded.", "Ok, 25 modules loaded.", "Ok, 25 modules reloaded."] \message ->
        it (cs message) (ghciLoadSucceeded message `shouldBe` True)
    it "does not classify a failed load as successful" do
        ghciLoadSucceeded "Failed, no modules loaded." `shouldBe` False

tests :: Spec
tests = describe "IHP.IDE.Types.extractCrashMessage" $ do
    -- Joins lines the way the GHCi output is accumulated by the dev server.
    let output = ByteString.intercalate "\n"

    it "extracts a single-line crash message between the markers" $ do
        let input = output
                [ "[[IHP_APP_CRASHED_BEGIN]]"
                , "Environment variable TELEGRAM_OWNER_USER_ID is required"
                , "[[IHP_APP_CRASHED]]"
                ]
        extractCrashMessage input `shouldBe` ["Environment variable TELEGRAM_OWNER_USER_ID is required"]

    it "extracts a multi-line crash message including the call stack" $ do
        let input = output
                [ "[[IHP_APP_CRASHED_BEGIN]]"
                , "Environment variable TELEGRAM_OWNER_USER_ID is required"
                , "CallStack (from HasCallStack):"
                , "  error, called at ./IHP/Prelude.hs:106:17 in ihp:IHP.Prelude"
                , "[[IHP_APP_CRASHED]]"
                ]
        extractCrashMessage input `shouldBe`
                [ "Environment variable TELEGRAM_OWNER_USER_ID is required"
                , "CallStack (from HasCallStack):"
                , "  error, called at ./IHP/Prelude.hs:106:17 in ihp:IHP.Prelude"
                ]

    it "ignores build-log output printed before the crash" $ do
        let input = output
                [ "[131 of 132] Compiling Main"
                , "Ok, 131 modules loaded."
                , "[[IHP_APP_CRASHED_BEGIN]]"
                , "Prelude.read: no parse"
                , "[[IHP_APP_CRASHED]]"
                ]
        extractCrashMessage input `shouldBe` ["Prelude.read: no parse"]

    it "matches the markers even when prefixed (e.g. by the GHCi prompt)" $ do
        let input = output
                [ "IHP> [[IHP_APP_CRASHED_BEGIN]]"
                , "Environment variable API_KEY is required"
                , "[[IHP_APP_CRASHED]]"
                ]
        extractCrashMessage input `shouldBe` ["Environment variable API_KEY is required"]

    it "returns an empty list when there is no crash" $ do
        let input = output
                [ "[131 of 132] Compiling Main"
                , "Ok, 131 modules loaded."
                , "Server started"
                ]
        extractCrashMessage input `shouldBe` []
