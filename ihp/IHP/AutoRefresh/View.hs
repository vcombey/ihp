module IHP.AutoRefresh.View where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.LocalFirst.Types
import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html5 as Html5
import IHP.Controller.Context
import IHP.AutoRefresh (autoRefreshStateVaultKey)
import IHP.LocalFirst (localFirstStateVaultKey)
import qualified Data.Vault.Lazy as Vault

autoRefreshMeta :: (?context :: ControllerContext) => Html5.Html
autoRefreshMeta = case (autoRefreshState, localFirstState) of
    (Just AutoRefreshEnabled { sessionId }, Just localState@LocalFirstEnabled {}) ->
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
    (Just AutoRefreshEnabled { sessionId }, _) ->
        [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId}/>|]
    (_, Just localState@LocalFirstEnabled {}) ->
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
    where
        autoRefreshState = Vault.lookup autoRefreshStateVaultKey ?context.request.vault
        localFirstState = Vault.lookup localFirstStateVaultKey ?context.request.vault

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
