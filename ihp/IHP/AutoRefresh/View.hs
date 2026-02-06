module IHP.AutoRefresh.View where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html5 as Html5
import IHP.Controller.Context

autoRefreshMeta :: (?context :: ControllerContext) => Html5.Html
autoRefreshMeta = case maybeFromFrozenContext @AutoRefreshState of
        Nothing -> mempty
        Just AutoRefreshDisabled -> mempty
        Just AutoRefreshEnabled { sessionId } -> case maybeFromFrozenContext @AutoRefreshTarget of
            Just (AutoRefreshTarget target) -> [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId} data-ihp-auto-refresh-target={target}/>|]
            Nothing -> [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId}/>|]
