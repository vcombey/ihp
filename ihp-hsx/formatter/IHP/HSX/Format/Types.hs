{-# LANGUAGE OverloadedStrings #-}

module IHP.HSX.Format.Types
    ( QuoteRegion (..)
    , ClosingStyle (..)
    , QuoteAttributeValue (..)
    , QuoteAttribute (..)
    , QuoteNode (..)
    ) where

import Prelude
import Data.Text (Text)

data QuoteRegion = QuoteRegion
    { name :: !Text
    , startIndex :: !Int
    , bodyStartIndex :: !Int
    , bodyEndIndex :: !Int
    , endIndex :: !Int
    , linePrefix :: !Text
    } deriving (Eq, Show)

data ClosingStyle
    = VoidLeaf
    | SelfClosing
    | ExplicitClosing
    deriving (Eq, Show)

data QuoteAttributeValue
    = TextValue !Text
    | ExpressionValue !Text
    deriving (Eq, Show)

data QuoteAttribute
    = BareAttribute !Text
    | StaticAttribute !Text !QuoteAttributeValue
    | SpreadAttribute !Text
    deriving (Eq, Show)

data QuoteNode
    = Element !Text ![QuoteAttribute] ![QuoteNode] !ClosingStyle
    | TextNode !Text
    | RawTextNode !Text
    | SpliceNode !Text
    | CommentNode !Text
    | NoRenderCommentNode !Text
    deriving (Eq, Show)
