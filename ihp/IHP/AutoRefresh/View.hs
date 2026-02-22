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
import qualified Data.Text as Text

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
                data-ihp-local-conflict-policy={localConflictPolicyText localState}
                data-ihp-local-conflict-field={localConflictFieldText localState}
                data-ihp-local-sync-tables={localSyncTablesText localState}
                data-ihp-local-auth-policy={localAuthPolicyText localState}
                data-ihp-local-schema-policy={localSchemaPolicyText localState}
                data-ihp-local-reconnect-probe-path={localReconnectProbePathText localState}
                data-ihp-local-reconnect-probe-timeout-ms={localReconnectProbeTimeoutText localState}
                data-ihp-local-reconnect-probe-interval-ms={localReconnectProbeIntervalText localState}
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
                data-ihp-local-conflict-policy={localConflictPolicyText localState}
                data-ihp-local-conflict-field={localConflictFieldText localState}
                data-ihp-local-sync-tables={localSyncTablesText localState}
                data-ihp-local-auth-policy={localAuthPolicyText localState}
                data-ihp-local-schema-policy={localSchemaPolicyText localState}
                data-ihp-local-reconnect-probe-path={localReconnectProbePathText localState}
                data-ihp-local-reconnect-probe-timeout-ms={localReconnectProbeTimeoutText localState}
                data-ihp-local-reconnect-probe-interval-ms={localReconnectProbeIntervalText localState}
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

        localConflictPolicyText :: LocalFirstState -> Text
        localConflictPolicyText LocalFirstEnabled { options = LocalOptions { conflictPolicy } } = case conflictPolicy of
            LocalConflictServerWins -> "server-wins"
            LocalConflictClientWins -> "client-wins"
            LocalConflictLastWriteWinsBy {} -> "last-write-wins"
        localConflictPolicyText LocalFirstDisabled = ""

        localConflictFieldText :: LocalFirstState -> Text
        localConflictFieldText LocalFirstEnabled { options = LocalOptions { conflictPolicy } } = case conflictPolicy of
            LocalConflictLastWriteWinsBy field -> field
            _ -> ""
        localConflictFieldText LocalFirstDisabled = ""

        localAuthPolicyText :: LocalFirstState -> Text
        localAuthPolicyText LocalFirstEnabled { options = LocalOptions { authPolicy } } = case authPolicy of
            LocalAuthLastAuthenticatedUser -> "last-authenticated-user"
        localAuthPolicyText LocalFirstDisabled = ""

        localSchemaPolicyText :: LocalFirstState -> Text
        localSchemaPolicyText LocalFirstEnabled { options = LocalOptions { schemaPolicy } } = case schemaPolicy of
            LocalSchemaWholeApp -> "whole-app"
        localSchemaPolicyText LocalFirstDisabled = ""

        localSyncTablesText :: LocalFirstState -> Text
        localSyncTablesText LocalFirstEnabled { options = LocalOptions { syncTables } } =
            syncTables |> Text.intercalate ","
        localSyncTablesText LocalFirstDisabled = ""

        localReconnectProbePathText :: LocalFirstState -> Text
        localReconnectProbePathText LocalFirstEnabled { options = LocalOptions { reconnectPolicy = LocalReconnectPolicy { probePath } } } =
            fromMaybe "" probePath
        localReconnectProbePathText LocalFirstDisabled = ""

        localReconnectProbeTimeoutText :: LocalFirstState -> Text
        localReconnectProbeTimeoutText LocalFirstEnabled { options = LocalOptions { reconnectPolicy = LocalReconnectPolicy { probeTimeoutMs } } } =
            tshow probeTimeoutMs
        localReconnectProbeTimeoutText LocalFirstDisabled = ""

        localReconnectProbeIntervalText :: LocalFirstState -> Text
        localReconnectProbeIntervalText LocalFirstEnabled { options = LocalOptions { reconnectPolicy = LocalReconnectPolicy { probeIntervalMs } } } =
            tshow probeIntervalMs
        localReconnectProbeIntervalText LocalFirstDisabled = ""
