{-# LANGUAGE BangPatterns #-}
module IHP.Controller.Render where

import ClassyPrelude
import qualified Data.Aeson
import qualified IHP.Controller.Context as Context
import IHP.AutoRefresh.View (autoRefreshMeta)
import IHP.Controller.Layout
import IHP.ControllerSupport
import IHP.FlashMessages (consumeFlashMessagesMiddleware)
import IHP.HSX.Markup (Markup, MarkupM(..))
import IHP.RouterSupport (validateOpenApiRenderedView)
import qualified IHP.ViewSupport as ViewSupport
import Network.HTTP.Types (Status, status200, status406)
import Network.HTTP.Types.Header
import qualified Network.HTTP.Media as Accept
import Network.Wai (responseBuilder, responseFile, responseLBS)

renderPlain :: (?request :: Request, ?respond :: Respond) => LByteString -> IO ResponseReceived
renderPlain text = respondWith $ responseLBS status200 [(hContentType, "text/plain")] text
{-# INLINE renderPlain #-}

respondHtml :: (?request :: Request, ?respond :: Respond) => Markup -> IO ResponseReceived
respondHtml (Markup builder) =
    respondWith $ responseBuilder status200 [(hContentType, "text/html; charset=utf-8"), (hConnection, "keep-alive")] builder
{-# INLINE respondHtml #-}

-- | Like 'respondHtml', but always prepends 'autoRefreshMeta' to the response body.
--
-- Intended for fragment-style responses (e.g. HTMX) where a full layout is not rendered.
respondHtmlFragment :: (?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => Markup -> IO ResponseReceived
respondHtmlFragment html = do
    frozenContext <- Context.freeze ?context
    let ?context = frozenContext
    let Markup builder = autoRefreshMeta <> html
    respondWith $ responseBuilder status200 [(hContentType, "text/html; charset=utf-8"), (hConnection, "keep-alive")] builder
{-# INLINE respondHtmlFragment #-}

respondSvg :: (?request :: Request, ?respond :: Respond) => Markup -> IO ResponseReceived
respondSvg (Markup builder) =
    respondWith $ responseBuilder status200 [(hContentType, "image/svg+xml"), (hConnection, "keep-alive")] builder
{-# INLINABLE respondSvg #-}

renderHtml :: forall view. (ViewSupport.View view, ?context :: ControllerContext, ?request :: Request) => view -> IO Markup
renderHtml !view = do
    let ?view = view
    ViewSupport.beforeRender view
    frozenContext <- Context.freeze ?context

    let ?context = frozenContext
    (ViewLayout layout) <- getLayout

    let boundHtml = let ?context = frozenContext in layout (ViewSupport.html ?view)
    pure boundHtml
{-# INLINE renderHtml #-}

-- | Like 'renderHtml', but does not apply the current layout.
--
-- Useful for endpoint fragments that should return only partial HTML.
renderHtmlFragment :: forall view. (ViewSupport.View view, ?context :: ControllerContext, ?request :: Request) => view -> IO Markup
renderHtmlFragment !view = do
    let ?view = view
    ViewSupport.beforeRender view
    frozenContext <- Context.freeze ?context

    let ?context = frozenContext
    pure (ViewSupport.html ?view)
{-# INLINE renderHtmlFragment #-}

renderFile :: (?request :: Request, ?respond :: Respond) => String -> ByteString -> IO ResponseReceived
renderFile filePath contentType = respondWith $ responseFile status200 [(hContentType, contentType)] filePath Nothing
{-# INLINE renderFile #-}

renderJson :: (?request :: Request, ?respond :: Respond) => Data.Aeson.ToJSON json => json -> IO ResponseReceived
renderJson json = renderJsonWithStatusCode status200 json
{-# INLINE renderJson #-}

renderJsonWithStatusCode :: (?request :: Request, ?respond :: Respond) => Data.Aeson.ToJSON json => Status -> json -> IO ResponseReceived
renderJsonWithStatusCode statusCode json = respondWith $ responseLBS statusCode [(hContentType, "application/json")] (Data.Aeson.encode json)
{-# INLINE renderJsonWithStatusCode #-}

renderXml :: (?request :: Request, ?respond :: Respond) => LByteString -> IO ResponseReceived
renderXml xml = respondWith $ responseLBS status200 [(hContentType, "application/xml")] xml
{-# INLINE renderXml #-}

-- | Use 'setHeader' instead
renderJson' :: (?request :: Request, ?respond :: Respond) => ResponseHeaders -> Data.Aeson.ToJSON json => json -> IO ResponseReceived
renderJson' additionalHeaders json = respondWith $ responseLBS status200 ([(hContentType, "application/json")] <> additionalHeaders) (Data.Aeson.encode json)
{-# INLINE renderJson' #-}

data PolymorphicRender
    = PolymorphicRender
        { html :: Maybe (IO ResponseReceived)
        , json :: Maybe (IO ResponseReceived)
        }

-- | Can be used to render different responses for html, json, etc. requests based on the Accept header.
renderPolymorphic :: (?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => PolymorphicRender -> IO ResponseReceived
renderPolymorphic PolymorphicRender { html, json } = do
    let acceptHeader = lookup hAccept (request.requestHeaders)
    case acceptHeader of
        Nothing | Just handler <- html -> handler
        Just h | "text/html" `isPrefixOf` h, Just handler <- html -> handler
        _ -> do
            let accept = fromMaybe "text/html" acceptHeader
            let send406Error = respondWith $ responseLBS status406 [] "Could not find any acceptable response format"
            let formats = concat
                    [ case html of
                        Just handler -> [("text/html", handler)]
                        Nothing -> []
                    , case json of
                        Just handler -> [("application/json", handler)]
                        Nothing -> []
                    ]
            fromMaybe send406Error (Accept.mapAcceptMedia formats accept)
{-# INLINE renderPolymorphic #-}

polymorphicRender :: PolymorphicRender
polymorphicRender = PolymorphicRender Nothing Nothing

{-# INLINE render #-}
render
    :: forall view.
        ( ViewSupport.View view
        , ?context :: ControllerContext
        , ?request :: Request
        , ?respond :: Respond
        )
    => view
    -> IO ResponseReceived
render !view = do
    let !currentRequest = ?request
    renderHtmlView currentRequest view

{-# INLINE renderHtmlOrJson #-}
renderHtmlOrJson
    :: forall view.
        ( ViewSupport.View view
        , ViewSupport.JsonView view
        , Typeable view
        , ?context :: ControllerContext
        , ?request :: Request
        , ?respond :: Respond
        )
    => view
    -> IO ResponseReceived
renderHtmlOrJson !view = do
    let !currentRequest = ?request
    renderPolymorphic PolymorphicRender
        { html = Just (renderHtmlView currentRequest view)
        , json = Just do
                let jsonValue = ViewSupport.json view
                validateOpenApiRenderedView view jsonValue
                renderJson jsonValue
        }

-- | Render a view fragment without layout and respond with 'autoRefreshMeta' prepended.
renderFragment :: forall view. (ViewSupport.View view, ?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => view -> IO ResponseReceived
renderFragment !view = renderHtmlFragment view >>= respondHtmlFragment
{-# INLINE renderFragment #-}

renderHtmlView :: (ViewSupport.View view, ?context :: ControllerContext, ?respond :: Respond) => Request -> view -> IO ResponseReceived
renderHtmlView currentRequest view = do
    let next request respond = do
            let ?request = request
            let ?respond = respond
            renderHtml view >>= respondHtml
    consumeFlashMessagesMiddleware next currentRequest ?respond
