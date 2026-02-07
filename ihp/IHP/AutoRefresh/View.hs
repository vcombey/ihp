module IHP.AutoRefresh.View where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.LocalFirst.Types
import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html5 as Html5
import IHP.Controller.Context

autoRefreshMeta :: (?context :: ControllerContext) => Html5.Html
autoRefreshMeta = case maybeFromFrozenContext @AutoRefreshState of
        Nothing -> renderLocalOnlyMeta
        Just AutoRefreshDisabled -> renderLocalOnlyMeta
        Just AutoRefreshEnabled { sessionId } ->
            case maybeFromFrozenContext @LocalFirstState of
                Just localState@LocalFirstEnabled {} ->
                    [hsx|
                        <meta
                            property="ihp-auto-refresh-id"
                            content={tshow sessionId}
                            data-ihp-local-route-id={localRouteIdText localState}
                            data-ihp-local-route={localRoutePathText localState}
                            data-ihp-local-sync-policy={localSyncPolicyText localState}
                            data-ihp-local-auth-policy={localAuthPolicyText localState}
                            data-ihp-local-schema-policy={localSchemaPolicyText localState}
                        />
                    |]
                _ ->
                    [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId}/>|]
    where
        renderLocalOnlyMeta = case maybeFromFrozenContext @LocalFirstState of
            Just localState@LocalFirstEnabled {} ->
                [hsx|
                    <meta
                        property={localMetaPropertyText}
                        content={localRoutePathText localState}
                        data-ihp-local-route-id={localRouteIdText localState}
                        data-ihp-local-sync-policy={localSyncPolicyText localState}
                        data-ihp-local-auth-policy={localAuthPolicyText localState}
                        data-ihp-local-schema-policy={localSchemaPolicyText localState}
                    />
                |]
            _ -> mempty

        localMetaPropertyText :: Text
        localMetaPropertyText = cs localMetaProperty

        localRouteIdText :: LocalFirstState -> Text
        localRouteIdText LocalFirstEnabled { routeId } = tshow routeId
        localRouteIdText LocalFirstDisabled = ""

        localRoutePathText :: LocalFirstState -> Text
        localRoutePathText LocalFirstEnabled { routePath = LocalRoutePath routePath } = routePath
        localRoutePathText LocalFirstDisabled = ""

        localSyncPolicyText :: LocalFirstState -> Text
        localSyncPolicyText LocalFirstEnabled { options = LocalOptions { syncPolicy } } = case syncPolicy of
            LocalSyncServerWins -> "server-wins"
        localSyncPolicyText LocalFirstDisabled = ""

        localAuthPolicyText :: LocalFirstState -> Text
        localAuthPolicyText LocalFirstEnabled { options = LocalOptions { authPolicy } } = case authPolicy of
            LocalAuthLastAuthenticatedUser -> "last-authenticated-user"
        localAuthPolicyText LocalFirstDisabled = ""

        localSchemaPolicyText :: LocalFirstState -> Text
        localSchemaPolicyText LocalFirstEnabled { options = LocalOptions { schemaPolicy } } = case schemaPolicy of
            LocalSchemaWholeApp -> "whole-app"
        localSchemaPolicyText LocalFirstDisabled = ""
