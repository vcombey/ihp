{-# LANGUAGE TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
Module: IHP.HSX.Lucid2.QQ
Description: Defines the @[hsx||]@ and @[hsxM||]@ syntax
Copyright: (c) digitally induced GmbH, 2025
-}
module IHP.HSX.Lucid2.QQ
  ( hsx
  , uncheckedHsx
  , customHsx
  , hsxExpression
  , uncheckedHsxExpression
  , customHsxExpression
  , quoteHsxExpression
  , expandHsxQuasiQuote
  , hsxM
  , uncheckedHsxM
  , customHsxM
  , hsxExpressionM
  , uncheckedHsxExpressionM
  , customHsxExpressionM
  , quoteHsxExpressionM
  ) where

import           Prelude
import Control.Applicative ((<|>))
import Data.Foldable (Foldable(..))
import Data.Text (Text)
import qualified Data.Text as Text
import           IHP.HSX.Parser
import           IHP.HSX.Lucid2.Attribute
import qualified IHP.HSX.Lucid2.ToHtml as M
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax           as TH
import           Language.Haskell.TH.Quote
import Data.String.Conversions
import qualified Text.Megaparsec as Megaparsec
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import Lucid.Html5 (doctype_)
import Lucid.Base
  ( Attributes
  , Html
  , HtmlT
  , ToHtml (..)
  , makeElement
  , makeElementNoEnd
  )

hsx :: QuasiQuoter
hsx = customHsx
        (HsxSettings
            { checkMarkup = True
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            , expandQuasiQuote = expandHsxQuasiQuote
            }
        )

uncheckedHsx :: QuasiQuoter
uncheckedHsx = customHsx
        (HsxSettings
            { checkMarkup = False
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            , expandQuasiQuote = expandHsxQuasiQuote
            }
        )

customHsx :: HsxSettings -> QuasiQuoter
customHsx settings =
    QuasiQuoter
        { quoteExp = quoteHsxExpression settings
        , quotePat = error "quotePat: not defined"
        , quoteDec = error "quoteDec: not defined"
        , quoteType = error "quoteType: not defined"
        }

-- | Parse a static HSX snippet from a string and return Lucid HTML.
--
-- This is useful inside @{...}@ expressions when nesting another @[hsx|...|]@
-- is not possible because @|]@ would close the outer quasiquote.
hsxExpression :: String -> Html ()
hsxExpression = customHsxExpression defaultSettings

uncheckedHsxExpression :: String -> Html ()
uncheckedHsxExpression = customHsxExpression uncheckedSettings

customHsxExpression :: HsxSettings -> String -> Html ()
customHsxExpression settings code =
    case renderStaticHsx settings code of
        Left renderError -> error (cs renderError)
        Right htmlCode -> toHtmlRaw htmlCode

quoteHsxExpression :: HsxSettings -> String -> TH.ExpQ
quoteHsxExpression settings code = do
        hsxPosition <- findHSXPosition
        extensions <- TH.extsEnabled
        quoteHsxExpressionAtWithExtensions settings hsxPosition extensions code
    where

        findHSXPosition = do
            loc <- TH.location
            let (line, col) = TH.loc_start loc
            pure $ Megaparsec.SourcePos (TH.loc_filename loc) (Megaparsec.mkPos line) (Megaparsec.mkPos col)

quoteHsxExpressionAtWithExtensions :: HsxSettings -> Megaparsec.SourcePos -> [TH.Extension] -> String -> TH.ExpQ
quoteHsxExpressionAtWithExtensions settings hsxPosition extensions code = do
        let settings' = settings { expandQuasiQuote = composeExpandQuasiQuote extensions settings }
        expression <- case parseHsx settings' hsxPosition extensions (cs code) of
                Left error   -> fail (Megaparsec.errorBundlePretty error)
                Right result -> pure result
        compileToHaskell expression

compileToHaskell :: Node -> TH.ExpQ
compileToHaskell (Node name [StaticAttribute "html" (TextValue "html")] [] True)
  | Text.toCaseFold name == "!doctype" = [| doctype_ |]
compileToHaskell (Node name attributes children isLeaf) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
        listAttributes = TH.listE $ map toLucidAttributes attributes
    in
        if isLeaf
            then
                let
                    element = nodeToLucidLeaf name
                in
                    [| $element $listAttributes |]
            else
                let
                    element = nodeToLucidElement name
                in [| $element $listAttributes (sequence_ @[] $renderedChildren) |]
compileToHaskell (Children children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in [| (sequence_ @[] $(renderedChildren)) |]
compileToHaskell (FragmentNode children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in [| (sequence_ @[] $(renderedChildren)) |]

compileToHaskell (TextNode value) = [| toHtmlRaw value |]
compileToHaskell (PreEscapedTextNode value) = [| toHtmlRaw value |]
compileToHaskell (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToHaskell (CommentNode value) = [| toHtmlRaw @Text "<!--" >> toHtmlRaw value >> toHtmlRaw @Text "-->" |]
compileToHaskell NoRenderCommentNode = [| pure () |]

nodeToLucidElement :: Text -> TH.Q TH.Exp
nodeToLucidElement name =
    [| makeElement $(TH.lift name) |]

nodeToLucidLeaf :: Text -> TH.Q TH.Exp
nodeToLucidLeaf name =
    [| makeElementNoEnd $(TH.lift name) |]

toLucidAttributes :: Attribute -> TH.ExpQ
toLucidAttributes (StaticAttribute name (TextValue value)) =
    [| buildAttribute name value |]
toLucidAttributes (StaticAttribute name (ExpressionValue expression)) =
    [| buildAttribute name $(pure expression) |]
toLucidAttributes (SpreadAttributes expression) =
    [| spreadAttributes $(pure expression) |]

spreadAttributes :: (LucidAttributeValue lav) => [(Text, lav)] -> Attributes
spreadAttributes = foldMap' (uncurry buildAttribute)



-- Monad Version
hsxM :: QuasiQuoter
hsxM = customHsxM
        (HsxSettings
            { checkMarkup = True
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            , expandQuasiQuote = expandHsxQuasiQuote
            }
        )

uncheckedHsxM :: QuasiQuoter
uncheckedHsxM = customHsxM
        (HsxSettings
            { checkMarkup = False
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            , expandQuasiQuote = expandHsxQuasiQuote
            }
        )

customHsxM :: HsxSettings -> QuasiQuoter
customHsxM settings =
    QuasiQuoter
        { quoteExp = quoteHsxExpressionM settings
        , quotePat = error "quotePat: not defined"
        , quoteDec = error "quoteDec: not defined"
        , quoteType = error "quoteType: not defined"
        }

hsxExpressionM :: Monad m => String -> M.HtmlType (HtmlT m)
hsxExpressionM = customHsxExpressionM defaultSettings

uncheckedHsxExpressionM :: Monad m => String -> M.HtmlType (HtmlT m)
uncheckedHsxExpressionM = customHsxExpressionM uncheckedSettings

customHsxExpressionM :: Monad m => HsxSettings -> String -> M.HtmlType (HtmlT m)
customHsxExpressionM settings code =
    case renderStaticHsx settings code of
        Left renderError -> error (cs renderError)
        Right htmlCode -> M.Lucid2Html (toHtmlRaw htmlCode)

quoteHsxExpressionM :: HsxSettings -> String -> TH.ExpQ
quoteHsxExpressionM settings code = do
        hsxPosition <- findHSXPosition
        extensions <- TH.extsEnabled
        quoteHsxExpressionMAtWithExtensions settings hsxPosition extensions code
    where

        findHSXPosition = do
            loc <- TH.location
            let (line, col) = TH.loc_start loc
            pure $ Megaparsec.SourcePos (TH.loc_filename loc) (Megaparsec.mkPos line) (Megaparsec.mkPos col)

quoteHsxExpressionMAtWithExtensions :: HsxSettings -> Megaparsec.SourcePos -> [TH.Extension] -> String -> TH.ExpQ
quoteHsxExpressionMAtWithExtensions settings hsxPosition extensions code = do
        let settings' = settings { expandQuasiQuote = composeExpandQuasiQuote extensions settings }
        expression <- case parseHsx settings' hsxPosition extensions (cs code) of
                Left error   -> fail (Megaparsec.errorBundlePretty error)
                Right result -> pure result
        [| M.unHtmlType $(compileToHaskellM expression) |]

composeExpandQuasiQuote :: [TH.Extension] -> HsxSettings -> Megaparsec.SourcePos -> String -> String -> Maybe TH.Exp
composeExpandQuasiQuote extensions settings sourcePos name body =
    expandHsxQuasiQuoteWithExtensions extensions sourcePos name body
        <|> expandQuasiQuote settings sourcePos name body

defaultSettings :: HsxSettings
defaultSettings =
    HsxSettings
        { checkMarkup = True
        , additionalTagNames = Set.empty
        , additionalAttributeNames = Set.empty
        , expandQuasiQuote = expandHsxQuasiQuote
        }

uncheckedSettings :: HsxSettings
uncheckedSettings =
    HsxSettings
        { checkMarkup = False
        , additionalTagNames = Set.empty
        , additionalAttributeNames = Set.empty
        , expandQuasiQuote = expandHsxQuasiQuote
        }

expandHsxQuasiQuote :: Megaparsec.SourcePos -> String -> String -> Maybe TH.Exp
expandHsxQuasiQuote = expandHsxQuasiQuoteWithExtensions []

expandHsxQuasiQuoteWithExtensions :: [TH.Extension] -> Megaparsec.SourcePos -> String -> String -> Maybe TH.Exp
expandHsxQuasiQuoteWithExtensions extensions sourcePos name body =
    case baseName name of
        "hsx" -> Just (runQExp (expandWithSettings defaultSettings compileToHaskell))
        "uncheckedHsx" -> Just (runQExp (expandWithSettings uncheckedSettings compileToHaskell))
        "hsxM" -> Just (runQExp (expandWithSettings defaultSettings compileToHaskellMExp))
        "uncheckedHsxM" -> Just (runQExp (expandWithSettings uncheckedSettings compileToHaskellMExp))
        _ -> Nothing
  where
    expandWithSettings settings compileNode = do
        let settings' = settings { expandQuasiQuote = expandHsxQuasiQuoteWithExtensions extensions }
        expression <- case parseHsx settings' sourcePos extensions (cs body) of
            Left parseError -> error (Megaparsec.errorBundlePretty parseError)
            Right result -> pure result
        compileNode expression

    compileToHaskellMExp expression = [| M.unHtmlType $(compileToHaskellM expression) |]

runQExp :: TH.ExpQ -> TH.Exp
runQExp q = unsafePerformIO (TH.runQ q)
{-# NOINLINE runQExp #-}

baseName :: String -> String
baseName = reverse . takeWhile (/= '.') . reverse

helperSourcePos :: Megaparsec.SourcePos
helperSourcePos = Megaparsec.SourcePos "<hsxExpression>" (Megaparsec.mkPos 1) (Megaparsec.mkPos 1)

renderStaticHsx :: HsxSettings -> String -> Either String Text
renderStaticHsx settings code =
    case parseHsx settings helperSourcePos [] (cs code) of
        Left parseError ->
            Left ("hsxExpression parse error:\n" <> Megaparsec.errorBundlePretty parseError)
        Right parsedNode ->
            case renderStaticNode parsedNode of
                Left renderError ->
                    Left ("hsxExpression supports only static HSX (no {..} splices): " <> cs renderError)
                Right htmlCode ->
                    Right htmlCode

compileToHaskellM :: Node -> TH.ExpQ
compileToHaskellM (Node name [StaticAttribute "html" (TextValue "html")] [] True)
  | Text.toCaseFold name == "!doctype" = [| M.Lucid2Html doctype_ |]
compileToHaskellM (Node name attributes children isLeaf) =
    let
        renderedChildren = TH.listE $ map compileToHaskellM children
        listAttributes = TH.listE $ map toLucidAttributes attributes
    in
        if isLeaf
            then
                let
                    element = nodeToLucidLeafM name
                in
                    [| $element $listAttributes |]
            else
                let
                    element = nodeToLucidElementM name
                in [| $element $listAttributes (M.sequenceChildren $renderedChildren) |]
compileToHaskellM (Children children) =
    let
        renderedChildren = TH.listE $ map compileToHaskellM children
    in [| (M.sequenceChildren $(renderedChildren)) |]
compileToHaskellM (FragmentNode children) =
    let
        renderedChildren = TH.listE $ map compileToHaskellM children
    in [| (M.sequenceChildren $(renderedChildren)) |]

compileToHaskellM (TextNode value) = [| M.toHtmlRaw value |]
compileToHaskellM (PreEscapedTextNode value) = [| M.toHtmlRaw value |]
compileToHaskellM (SplicedNode expression) = [| M.toHtml $(pure expression) |]
compileToHaskellM (CommentNode value) =
  [| M.sequenceChildren [M.toHtmlRaw @_ @Text "<!--", M.toHtmlRaw value, M.toHtmlRaw @_ @Text "-->"] |]
compileToHaskellM NoRenderCommentNode = [| M.Lucid2Html (pure ()) |]

nodeToLucidElementM :: Text -> TH.Q TH.Exp
nodeToLucidElementM name =
    [| M.makeElement $(TH.lift name) |]

nodeToLucidLeafM :: Text -> TH.Q TH.Exp
nodeToLucidLeafM name =
    [| M.makeElementNoEnd $(TH.lift name) |]
