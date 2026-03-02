(function () {
    var LOCAL_USER_KEY = 'ihp_local_user_id';
    var LOCAL_QUEUE_KEY_PREFIX = 'ihp_local_queue:';
    var LOCAL_FAILED_QUEUE_KEY_PREFIX = 'ihp_local_failed_queue:';
    var LOCAL_ACTION_QUEUE_KEY_PREFIX = 'ihp_local_action_queue:';
    var LOCAL_DB_PREFIX = 'idb://ihp-local-db:';
    var LOCAL_DEBUG_FLAG_KEY = 'ihp_local_debug';
    var LOCAL_DEBUG_PANEL_FLAG_KEY = 'ihp_local_debug_panel';
    var LOCAL_DEBUG_PANEL_ID = 'ihp-local-debug-panel';
    var LOCAL_DEBUG_PANEL_STYLE_ID = 'ihp-local-debug-panel-style';
    var LOCAL_RUNTIME_LOG_PREFIX = '[ihp-local-runtime]';
    var LOCAL_RENDERERS = {};
    var LOCAL_ACTIONS = {};
    var LOCAL_DOM_SNAPSHOTS = {};
    var LOCAL_CONFLICT_RESOLVERS = {};
    var localActionFormInterceptorInstalled = false;
    var localDebugPanelRefreshTimer = null;

    function isLocalDebugEnabled() {
        try {
            var flag = localStorage.getItem(LOCAL_DEBUG_FLAG_KEY);
            if (flag === null || flag === undefined) {
                return true;
            }
            var normalized = String(flag).trim().toLowerCase();
            return normalized !== '0' && normalized !== 'false' && normalized !== 'off';
        } catch (_error) {
            return true;
        }
    }

    function debugLog(message, details) {
        if (!isLocalDebugEnabled()) {
            return;
        }
        if (details === undefined) {
            console.log(LOCAL_RUNTIME_LOG_PREFIX + ' ' + message);
            return;
        }
        console.log(LOCAL_RUNTIME_LOG_PREFIX + ' ' + message, details);
    }

    function debugWarn(message, details) {
        if (!isLocalDebugEnabled()) {
            return;
        }
        if (details === undefined) {
            console.warn(LOCAL_RUNTIME_LOG_PREFIX + ' ' + message);
            return;
        }
        console.warn(LOCAL_RUNTIME_LOG_PREFIX + ' ' + message, details);
    }

    function debugError(message, error, context) {
        if (!isLocalDebugEnabled()) {
            return;
        }
        var payload = { error: error };
        if (context && typeof context === 'object') {
            payload.context = context;
        }
        console.error(LOCAL_RUNTIME_LOG_PREFIX + ' ' + message, payload);
    }

    function requestOnPageDebugPanelRefresh() {
        if (typeof document === 'undefined') {
            return;
        }
        document.dispatchEvent(new CustomEvent('ihp:local-debug:refresh'));
    }

    function isOnPageDebugPanelEnabled() {
        try {
            var flag = localStorage.getItem(LOCAL_DEBUG_PANEL_FLAG_KEY);
            if (flag === null || flag === undefined) {
                return true;
            }
            var normalized = String(flag).trim().toLowerCase();
            return normalized !== '0' && normalized !== 'false' && normalized !== 'off';
        } catch (_error) {
            return true;
        }
    }

    function setOnPageDebugPanelEnabled(enabled) {
        try {
            localStorage.setItem(LOCAL_DEBUG_PANEL_FLAG_KEY, enabled ? '1' : '0');
        } catch (_error) {}
    }

    function parseJson(value, fallback) {
        try {
            return JSON.parse(value);
        } catch (error) {
            debugWarn('Failed to parse JSON payload, using fallback', { value: value, error: String(error) });
            return fallback;
        }
    }

    function getCurrentUserId() {
        return localStorage.getItem(LOCAL_USER_KEY) || 'anonymous';
    }

    function setCurrentUserId(userId) {
        if (!userId) {
            debugLog('Clearing current local user id');
            localStorage.removeItem(LOCAL_USER_KEY);
            requestOnPageDebugPanelRefresh();
            return;
        }
        debugLog('Setting current local user id', { userId: userId });
        localStorage.setItem(LOCAL_USER_KEY, userId);
        requestOnPageDebugPanelRefresh();
    }

    function queueKey() {
        return LOCAL_QUEUE_KEY_PREFIX + getCurrentUserId();
    }

    function failedQueueKey() {
        return LOCAL_FAILED_QUEUE_KEY_PREFIX + getCurrentUserId();
    }

    function actionQueueKey() {
        return LOCAL_ACTION_QUEUE_KEY_PREFIX + getCurrentUserId();
    }

    function readQueue() {
        return parseJson(localStorage.getItem(queueKey()) || '[]', []);
    }

    function writeQueue(queue) {
        debugLog('Writing mutation queue', { length: withArray(queue).length });
        localStorage.setItem(queueKey(), JSON.stringify(queue));
        requestOnPageDebugPanelRefresh();
    }

    function readFailedQueue() {
        return parseJson(localStorage.getItem(failedQueueKey()) || '[]', []);
    }

    function writeFailedQueue(queue) {
        debugLog('Writing failed queue', { length: withArray(queue).length });
        localStorage.setItem(failedQueueKey(), JSON.stringify(queue));
        requestOnPageDebugPanelRefresh();
    }

    function readActionQueue() {
        return parseJson(localStorage.getItem(actionQueueKey()) || '[]', []);
    }

    function writeActionQueue(queue) {
        debugLog('Writing action queue', { length: withArray(queue).length });
        localStorage.setItem(actionQueueKey(), JSON.stringify(queue));
        requestOnPageDebugPanelRefresh();
    }

    function appendFailedMutation(item) {
        var queue = readFailedQueue();
        queue.push(item);
        writeFailedQueue(queue);
        debugWarn('Appended failed mutation', {
            queueLength: queue.length,
            payload: item && item.payload ? summarizePayload(item.payload) : null,
            response: item && item.response ? item.response : null,
        });
    }

    function appendQueuedMutation(item) {
        var queue = readQueue();
        queue.push(item);
        writeQueue(queue);
        debugLog('Appended mutation to replay queue', {
            queueLength: queue.length,
            payload: item && item.payload ? summarizePayload(item.payload) : null,
        });
    }

    function appendQueuedAction(item) {
        var queue = readActionQueue();
        queue.push(item);
        writeActionQueue(queue);
        debugLog('Appended action to replay queue', {
            queueLength: queue.length,
            routePath: item ? item.routePath : null,
            method: item ? item.method : null,
        });
    }

    function drainQueuedMutations() {
        var queue = readQueue();
        writeQueue([]);
        debugLog('Drained mutation replay queue', { drained: withArray(queue).length });
        return queue;
    }

    function localMeta() {
        var localMetaTag = document.querySelector('meta[property="ihp-local-route"]');
        if (localMetaTag) {
            return localMetaTag;
        }
        return document.querySelector('meta[property="ihp-auto-refresh-id"][data-ihp-local-route]');
    }

    function parseIntAttribute(value, fallback) {
        var parsed = Number.parseInt(String(value || ''), 10);
        if (!Number.isFinite(parsed) || parsed <= 0) {
            return fallback;
        }
        return parsed;
    }

    function parseCsvAttribute(value) {
        if (!value || typeof value !== 'string') {
            return [];
        }
        return value
            .split(',')
            .map(function (entry) { return entry.trim(); })
            .filter(function (entry) { return entry.length > 0; });
    }

    function readLocalRouteMetadata(options) {
        var shouldLog = !(options && options.log === false);
        var meta = localMeta();
        if (!meta) {
            if (shouldLog) {
                debugLog('No local-route meta tag found on page');
            }
            return null;
        }
        var syncTables = parseCsvAttribute(meta.getAttribute('data-ihp-local-sync-tables'));
        var reconnectProbePath = meta.getAttribute('data-ihp-local-reconnect-probe-path') || '';
        var conflictPolicy = meta.getAttribute('data-ihp-local-conflict-policy') || 'server-wins';
        var conflictField = meta.getAttribute('data-ihp-local-conflict-field') || '';
        var explicitRoutePath = meta.getAttribute('data-ihp-local-route');
        var metadata = {
            routePath: explicitRoutePath || meta.getAttribute('content'),
            routeId: meta.getAttribute('data-ihp-local-route-id') || null,
            syncPolicy: meta.getAttribute('data-ihp-local-sync-policy') || 'server-wins',
            conflictPolicy: conflictPolicy,
            conflictField: conflictField.length > 0 ? conflictField : null,
            syncTables: syncTables,
            authPolicy: meta.getAttribute('data-ihp-local-auth-policy') || 'last-authenticated-user',
            schemaPolicy: meta.getAttribute('data-ihp-local-schema-policy') || 'whole-app',
            reconnectProbePath: reconnectProbePath.length > 0 ? reconnectProbePath : null,
            reconnectProbeTimeoutMs: parseIntAttribute(meta.getAttribute('data-ihp-local-reconnect-probe-timeout-ms'), 2000),
            reconnectProbeIntervalMs: parseIntAttribute(meta.getAttribute('data-ihp-local-reconnect-probe-interval-ms'), 15000),
        };
        if (shouldLog) {
            debugLog('Read local-route metadata', metadata);
        }
        return metadata;
    }

    function isLocalRouteActive() {
        return !!readLocalRouteMetadata();
    }

    function userScopedDbPath() {
        return LOCAL_DB_PREFIX + getCurrentUserId();
    }

    function sanitizeDbKey(value) {
        return String(value).replace(/[^A-Za-z0-9_-]/g, '_');
    }

    function generateUUID() {
        if (typeof crypto !== 'undefined' && crypto.randomUUID) {
            return crypto.randomUUID();
        }
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (char) {
            var rand = Math.random() * 16 | 0;
            var value = char === 'x' ? rand : (rand & 0x3 | 0x8);
            return value.toString(16);
        });
    }

    function ensureRecordId(record) {
        if (!record || typeof record !== 'object') {
            return record;
        }
        if (record.id) {
            return record;
        }
        record.id = generateUUID();
        return record;
    }

    function supportsOpfsAccessHandles() {
        return typeof FileSystemFileHandle !== 'undefined'
            && FileSystemFileHandle
            && FileSystemFileHandle.prototype
            && typeof FileSystemFileHandle.prototype.createSyncAccessHandle === 'function';
    }

    function pickFilesystemCandidates() {
        var scopedUser = sanitizeDbKey(getCurrentUserId());
        var filesystems = [];
        if (typeof navigator !== 'undefined'
            && 'storage' in navigator
            && navigator.storage
            && navigator.storage.getDirectory
            && supportsOpfsAccessHandles()) {
            filesystems.push('opfs-ahp://ihp-local-' + scopedUser);
        }
        filesystems.push(userScopedDbPath());
        return filesystems;
    }

    function waitForPGlite(timeoutMs) {
        if (typeof window === 'undefined') {
            debugWarn('window is unavailable while waiting for PGlite');
            return Promise.resolve(null);
        }
        if (window.PGlite) {
            debugLog('PGlite constructor already available on window');
            return Promise.resolve(window.PGlite);
        }
        var waitTimeoutMs = Number.isFinite(timeoutMs) && timeoutMs > 0 ? timeoutMs : 5000;
        debugLog('Waiting for PGlite constructor', { timeoutMs: waitTimeoutMs });
        return new Promise(function (resolve) {
            var startedAt = Date.now();
            var timer = setInterval(function () {
                if (window.PGlite) {
                    clearInterval(timer);
                    debugLog('PGlite constructor became available', { waitedMs: Date.now() - startedAt });
                    resolve(window.PGlite);
                    return;
                }
                if ((Date.now() - startedAt) >= waitTimeoutMs) {
                    clearInterval(timer);
                    debugWarn('Timed out waiting for PGlite constructor', { waitedMs: Date.now() - startedAt });
                    resolve(null);
                }
            }, 50);
        });
    }

    async function openPGlite(dataDir, pgliteCtor) {
        debugLog('Opening local PGlite database', { dataDir: dataDir });
        var db = null;
        if (pgliteCtor && typeof pgliteCtor.create === 'function') {
            try {
                db = await pgliteCtor.create({ dataDir: dataDir });
                debugLog('Opened PGlite using static create()', { dataDir: dataDir });
            } catch (error) {
                debugWarn('PGlite.create() failed, falling back to constructor', {
                    dataDir: dataDir,
                    error: String(error && error.message ? error.message : error),
                });
                // Fall back to constructor-based initialization for compatibility.
            }
        }

        if (!db) {
            db = await new pgliteCtor(dataDir);
            debugLog('Opened PGlite using constructor', { dataDir: dataDir });
        }

        // Probe the DB early so we can fall back to a different filesystem backend
        // before returning a runtime that will fail on first real query.
        if (db && typeof db.query === 'function') {
            await db.query('SELECT 1');
        } else if (db && typeof db.exec === 'function') {
            await db.exec('SELECT 1');
        }
        debugLog('PGlite probe query succeeded', { dataDir: dataDir });

        return db;
    }

    function quoteIdentifier(identifier) {
        return '"' + String(identifier).replace(/"/g, '""') + '"';
    }

    function decodeDynamicValue(value) {
        if (value === null || value === undefined) {
            return null;
        }
        if (typeof value !== 'object' || !('tag' in value)) {
            return value;
        }
        switch (value.tag) {
            case 'TextValue':
            case 'UUIDValue':
            case 'DateTimeValue':
            case 'IntervalValue':
                return value.contents;
            case 'IntValue':
            case 'DoubleValue':
                return Number(value.contents);
            case 'BoolValue':
                return Boolean(value.contents);
            case 'ArrayValue':
                return (value.contents || []).map(decodeDynamicValue);
            case 'Null':
                return null;
            default:
                return value.contents;
        }
    }

    function decodeDataSyncRecord(record) {
        if (!record || typeof record !== 'object') {
            return record;
        }
        var decoded = {};
        Object.keys(record).forEach(function (key) {
            decoded[key] = decodeDynamicValue(record[key]);
        });
        return decoded;
    }

    function compileCondition(condition, parameters) {
        if (!condition) {
            return '1 = 1';
        }
        if (condition.tag === 'ColumnExpression') {
            return quoteIdentifier(condition.field);
        }
        if (condition.tag === 'LiteralExpression') {
            parameters.push(decodeDynamicValue(condition.value));
            return '?';
        }
        if (condition.tag === 'ListExpression') {
            var values = condition.values || [];
            if (values.length === 0) {
                return '(NULL)';
            }
            var placeholders = values.map(function (value) {
                parameters.push(decodeDynamicValue(value));
                return '?';
            }).join(', ');
            return '(' + placeholders + ')';
        }
        if (condition.tag === 'CallExpression') {
            if (condition.functionCall && condition.functionCall.tag === 'ToTSQuery') {
                parameters.push(condition.functionCall.text);
                return "to_tsquery('english', ?)";
            }
            return '1 = 1';
        }
        if (condition.tag === 'InfixOperatorExpression') {
            var left = compileCondition(condition.left, parameters);
            var right = compileCondition(condition.right, parameters);
            var op = condition.op;
            var mapped = '=';
            switch (op) {
                case 'OpEqual': mapped = '='; break;
                case 'OpGreaterThan': mapped = '>'; break;
                case 'OpLessThan': mapped = '<'; break;
                case 'OpGreaterThanOrEqual': mapped = '>='; break;
                case 'OpLessThanOrEqual': mapped = '<='; break;
                case 'OpNotEqual': mapped = '<>'; break;
                case 'OpAnd': mapped = 'AND'; break;
                case 'OpOr': mapped = 'OR'; break;
                case 'OpIs': mapped = 'IS'; break;
                case 'OpIsNot': mapped = 'IS NOT'; break;
                case 'OpIn': mapped = 'IN'; break;
                case 'OpTSMatch': mapped = '@@'; break;
            }
            return '(' + left + ') ' + mapped + ' (' + right + ')';
        }
        return '1 = 1';
    }

    function compileSelectQuery(query) {
        var parameters = [];
        var selectedColumns = '*';
        if (query.selectedColumns && query.selectedColumns.tag === 'SelectSpecific') {
            selectedColumns = (query.selectedColumns.contents || []).map(quoteIdentifier).join(', ');
            if (selectedColumns.length === 0) {
                selectedColumns = '*';
            }
        }

        var sql = 'SELECT ' + selectedColumns + ' FROM ' + quoteIdentifier(query.table);
        if (query.whereCondition) {
            sql += ' WHERE ' + compileCondition(query.whereCondition, parameters);
        }

        if (query.orderByClause && query.orderByClause.length > 0) {
            var orderBy = query.orderByClause
                .filter(function (clause) { return clause.orderByColumn; })
                .map(function (clause) {
                    var direction = clause.orderByDirection === 'Desc' ? 'DESC' : 'ASC';
                    return quoteIdentifier(clause.orderByColumn) + ' ' + direction;
                });
            if (orderBy.length > 0) {
                sql += ' ORDER BY ' + orderBy.join(', ');
            }
        }

        if (typeof query.limit === 'number') {
            sql += ' LIMIT ' + String(query.limit);
        }
        if (typeof query.offset === 'number') {
            sql += ' OFFSET ' + String(query.offset);
        }

        return { sql: sql, parameters: parameters };
    }

    function toRows(queryResult) {
        if (Array.isArray(queryResult)) {
            return queryResult;
        }
        if (queryResult && Array.isArray(queryResult.rows)) {
            return queryResult.rows;
        }
        return [];
    }

    function withArray(value) {
        if (Array.isArray(value)) {
            return value;
        }
        return [];
    }

    function deepClone(value) {
        return JSON.parse(JSON.stringify(value));
    }

    function summarizePayload(payload) {
        if (!payload || typeof payload !== 'object') {
            return payload;
        }
        var summary = {
            tag: payload.tag || null,
            requestId: payload.requestId,
        };
        if (payload.table) {
            summary.table = payload.table;
        }
        if (payload.id) {
            summary.id = payload.id;
        }
        if (payload.transactionId) {
            summary.transactionId = payload.transactionId;
        }
        if (Array.isArray(payload.records)) {
            summary.recordCount = payload.records.length;
        }
        if (payload.record && typeof payload.record === 'object') {
            summary.recordKeys = Object.keys(payload.record);
        }
        if (payload.patch && typeof payload.patch === 'object') {
            summary.patchKeys = Object.keys(payload.patch);
        }
        if (payload.query && payload.query.table) {
            summary.queryTable = payload.query.table;
        }
        return summary;
    }

    function queueLengths() {
        return {
            mutationQueue: withArray(readQueue()).length,
            actionQueue: withArray(readActionQueue()).length,
            failedQueue: withArray(readFailedQueue()).length,
        };
    }

    function ensureOnPageDebugPanelStyles() {
        if (typeof document === 'undefined') {
            return;
        }
        if (document.getElementById(LOCAL_DEBUG_PANEL_STYLE_ID)) {
            return;
        }
        var style = document.createElement('style');
        style.id = LOCAL_DEBUG_PANEL_STYLE_ID;
        style.textContent = ''
            + '#' + LOCAL_DEBUG_PANEL_ID + ' {'
            + ' position: fixed;'
            + ' right: 12px;'
            + ' bottom: 12px;'
            + ' width: 360px;'
            + ' max-width: calc(100vw - 24px);'
            + ' max-height: calc(100vh - 24px);'
            + ' z-index: 2147483647;'
            + ' background: rgba(17, 24, 39, 0.96);'
            + ' color: #f9fafb;'
            + ' border: 1px solid rgba(148, 163, 184, 0.4);'
            + ' border-radius: 10px;'
            + ' box-shadow: 0 10px 28px rgba(0, 0, 0, 0.45);'
            + ' font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace;'
            + ' font-size: 12px;'
            + ' line-height: 1.4;'
            + ' overflow: hidden;'
            + ' }'
            + '#' + LOCAL_DEBUG_PANEL_ID + ' .ihp-local-debug-header {'
            + ' display: flex;'
            + ' align-items: center;'
            + ' justify-content: space-between;'
            + ' gap: 8px;'
            + ' padding: 8px 10px;'
            + ' background: rgba(30, 41, 59, 0.96);'
            + ' border-bottom: 1px solid rgba(148, 163, 184, 0.3);'
            + ' }'
            + '#' + LOCAL_DEBUG_PANEL_ID + ' .ihp-local-debug-title {'
            + ' font-weight: 700;'
            + ' letter-spacing: 0.02em;'
            + ' white-space: nowrap;'
            + ' overflow: hidden;'
            + ' text-overflow: ellipsis;'
            + ' }'
            + '#' + LOCAL_DEBUG_PANEL_ID + ' .ihp-local-debug-actions {'
            + ' display: flex;'
            + ' gap: 6px;'
            + ' flex-wrap: wrap;'
            + ' justify-content: flex-end;'
            + ' }'
            + '#' + LOCAL_DEBUG_PANEL_ID + ' button {'
            + ' border: 1px solid rgba(148, 163, 184, 0.45);'
            + ' border-radius: 6px;'
            + ' background: rgba(15, 23, 42, 0.9);'
            + ' color: #f8fafc;'
            + ' padding: 2px 6px;'
            + ' font-size: 11px;'
            + ' cursor: pointer;'
            + ' }'
            + '#' + LOCAL_DEBUG_PANEL_ID + ' button:hover {'
            + ' background: rgba(30, 41, 59, 0.95);'
            + ' }'
            + '#' + LOCAL_DEBUG_PANEL_ID + ' .ihp-local-debug-body {'
            + ' padding: 8px 10px 10px;'
            + ' overflow: auto;'
            + ' max-height: min(55vh, 420px);'
            + ' }'
            + '#' + LOCAL_DEBUG_PANEL_ID + ' pre {'
            + ' margin: 0;'
            + ' white-space: pre-wrap;'
            + ' word-break: break-word;'
            + ' }'
            + '#' + LOCAL_DEBUG_PANEL_ID + '[data-sync-state="online"] .ihp-local-debug-title { color: #34d399; }'
            + '#' + LOCAL_DEBUG_PANEL_ID + '[data-sync-state="syncing"] .ihp-local-debug-title { color: #fbbf24; }'
            + '#' + LOCAL_DEBUG_PANEL_ID + '[data-sync-state="offline"] .ihp-local-debug-title { color: #f87171; }'
            + '#' + LOCAL_DEBUG_PANEL_ID + '[data-sync-state="error"] .ihp-local-debug-title { color: #fb7185; }';
        document.head.appendChild(style);
    }

    function collectRuntimeSnapshot(runtime) {
        var metadata = readLocalRouteMetadata({ log: false });
        var localQueueLengths = queueLengths();
        return {
            at: new Date().toISOString(),
            syncState: runtime.syncState,
            browserOnline: runtime.isBrowserOnline(),
            isReadyForLocal: runtime.isReadyForLocal(),
            metadata: metadata,
            queues: localQueueLengths,
            hasDb: !!runtime.db,
            schemaBootstrapped: runtime.schemaBootstrapped,
            hasRemoteDispatcher: !!runtime.remoteDispatcher,
            registeredActions: Object.keys(LOCAL_ACTIONS),
            registeredRenderers: Object.keys(LOCAL_RENDERERS),
            registeredDomSnapshots: Object.keys(LOCAL_DOM_SNAPSHOTS),
        };
    }

    function formatRuntimeSnapshot(snapshot) {
        var metadata = snapshot.metadata || {};
        var queues = snapshot.queues || {};
        return [
            'at: ' + snapshot.at,
            'syncState: ' + snapshot.syncState,
            'browserOnline: ' + String(snapshot.browserOnline),
            'readyForLocal: ' + String(snapshot.isReadyForLocal),
            'hasDb: ' + String(snapshot.hasDb) + ' | schemaBootstrapped: ' + String(snapshot.schemaBootstrapped),
            'hasRemoteDispatcher: ' + String(snapshot.hasRemoteDispatcher),
            'routePath: ' + (metadata.routePath || '-'),
            'routeId: ' + (metadata.routeId || '-'),
            'syncPolicy: ' + (metadata.syncPolicy || '-'),
            'conflictPolicy: ' + (metadata.conflictPolicy || '-'),
            'conflictField: ' + (metadata.conflictField || '-'),
            'syncTables: ' + ((metadata.syncTables && metadata.syncTables.length > 0) ? metadata.syncTables.join(', ') : '*'),
            'probe: ' + ((metadata.reconnectProbePath || '(current path)') + ' | timeout=' + String(metadata.reconnectProbeTimeoutMs || '-') + 'ms interval=' + String(metadata.reconnectProbeIntervalMs || '-') + 'ms'),
            'queues: mutations=' + String(queues.mutationQueue || 0) + ' actions=' + String(queues.actionQueue || 0) + ' failed=' + String(queues.failedQueue || 0),
            'actions: ' + (snapshot.registeredActions.length > 0 ? snapshot.registeredActions.join(', ') : '(none)'),
            'renderers: ' + (snapshot.registeredRenderers.length > 0 ? snapshot.registeredRenderers.join(', ') : '(none)'),
            'domSnapshots: ' + (snapshot.registeredDomSnapshots.length > 0 ? snapshot.registeredDomSnapshots.join(', ') : '(none)'),
        ].join('\n');
    }

    function removeOnPageDebugPanel() {
        if (typeof document === 'undefined') {
            return;
        }
        var panel = document.getElementById(LOCAL_DEBUG_PANEL_ID);
        if (panel && panel.parentNode) {
            panel.parentNode.removeChild(panel);
        }
        if (localDebugPanelRefreshTimer) {
            clearInterval(localDebugPanelRefreshTimer);
            localDebugPanelRefreshTimer = null;
        }
    }

    function refreshOnPageDebugPanel(runtime) {
        if (typeof document === 'undefined' || !isOnPageDebugPanelEnabled()) {
            return;
        }
        var panel = document.getElementById(LOCAL_DEBUG_PANEL_ID);
        if (!panel) {
            return;
        }
        var snapshot = collectRuntimeSnapshot(runtime);
        panel.setAttribute('data-sync-state', String(snapshot.syncState || 'idle'));

        var logButton = panel.querySelector('[data-action="toggle-logs"]');
        if (logButton) {
            logButton.textContent = runtime.isDebugLoggingEnabled() ? 'logs:on' : 'logs:off';
        }
        var panelButton = panel.querySelector('[data-action="toggle-panel"]');
        if (panelButton) {
            panelButton.textContent = isOnPageDebugPanelEnabled() ? 'panel:on' : 'panel:off';
        }
        var body = panel.querySelector('pre');
        if (body) {
            body.textContent = formatRuntimeSnapshot(snapshot);
        }
    }

    function mountOnPageDebugPanel(runtime) {
        if (typeof document === 'undefined') {
            return;
        }
        if (!isOnPageDebugPanelEnabled()) {
            removeOnPageDebugPanel();
            return;
        }

        ensureOnPageDebugPanelStyles();

        var panel = document.getElementById(LOCAL_DEBUG_PANEL_ID);
        if (!panel) {
            panel = document.createElement('section');
            panel.id = LOCAL_DEBUG_PANEL_ID;
            panel.innerHTML = ''
                + '<div class="ihp-local-debug-header">'
                + '  <div class="ihp-local-debug-title">Local First Debug</div>'
                + '  <div class="ihp-local-debug-actions">'
                + '    <button type="button" data-action="copy">copy</button>'
                + '    <button type="button" data-action="clear-queues">clear:q</button>'
                + '    <button type="button" data-action="toggle-logs">logs:on</button>'
                + '    <button type="button" data-action="toggle-panel">panel:on</button>'
                + '  </div>'
                + '</div>'
                + '<div class="ihp-local-debug-body"><pre></pre></div>';

            panel.addEventListener('click', function (event) {
                var button = event.target;
                if (!button || button.tagName !== 'BUTTON') {
                    return;
                }
                var action = button.getAttribute('data-action');
                if (action === 'copy') {
                    var snapshot = collectRuntimeSnapshot(runtime);
                    var serialized = JSON.stringify(snapshot, null, 2);
                    if (navigator.clipboard && typeof navigator.clipboard.writeText === 'function') {
                        navigator.clipboard.writeText(serialized).then(function () {
                            debugLog('Copied local-first debug snapshot to clipboard');
                        }).catch(function (error) {
                            debugError('Failed to copy local-first debug snapshot', error);
                        });
                    } else {
                        debugWarn('Clipboard API unavailable; cannot copy debug snapshot');
                    }
                    return;
                }
                if (action === 'clear-queues') {
                    writeQueue([]);
                    writeActionQueue([]);
                    writeFailedQueue([]);
                    debugLog('Cleared all local-first queues from debug panel');
                    requestOnPageDebugPanelRefresh();
                    return;
                }
                if (action === 'toggle-logs') {
                    runtime.setDebugLogging(!runtime.isDebugLoggingEnabled());
                    requestOnPageDebugPanelRefresh();
                    return;
                }
                if (action === 'toggle-panel') {
                    setOnPageDebugPanelEnabled(!isOnPageDebugPanelEnabled());
                    if (!isOnPageDebugPanelEnabled()) {
                        removeOnPageDebugPanel();
                        debugLog('Disabled on-page debug panel');
                        return;
                    }
                    mountOnPageDebugPanel(runtime);
                    requestOnPageDebugPanelRefresh();
                    debugLog('Enabled on-page debug panel');
                }
            });

            document.body.appendChild(panel);
            debugLog('Mounted on-page local-first debug panel');
        }

        refreshOnPageDebugPanel(runtime);
        if (localDebugPanelRefreshTimer) {
            clearInterval(localDebugPanelRefreshTimer);
        }
        localDebugPanelRefreshTimer = setInterval(function () {
            refreshOnPageDebugPanel(runtime);
        }, 1000);
    }

    function readJwtToken() {
        try {
            return localStorage.getItem('ihp_jwt');
        } catch (_error) {
            return null;
        }
    }

    function dataSyncWebSocketUrl() {
        var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
        var url = socketProtocol + '://' + window.location.host + '/DataSyncController';
        var jwt = readJwtToken();
        if (jwt) {
            url += '?access_token=' + encodeURIComponent(jwt);
        }
        debugLog('Computed DataSync websocket URL', { url: url, hasJwt: !!jwt });
        return url;
    }

    function sendDataSyncMessageOverWebSocket(payload) {
        debugLog('Sending replay payload over DataSync websocket', summarizePayload(payload));
        return new Promise(function (resolve, reject) {
            var socket;
            var settled = false;
            var timeout = setTimeout(function () {
                if (settled) {
                    return;
                }
                settled = true;
                try {
                    if (socket && socket.readyState === WebSocket.OPEN) {
                        socket.close();
                    }
                } catch (_error) {}
                debugWarn('DataSync replay timed out', summarizePayload(payload));
                reject(new Error('Timed out while replaying local mutation'));
            }, 5000);

            function finishWithError(error) {
                if (settled) {
                    return;
                }
                settled = true;
                clearTimeout(timeout);
                try {
                    if (socket && socket.readyState === WebSocket.OPEN) {
                        socket.close();
                    }
                } catch (_error) {}
                debugError('DataSync websocket replay failed', error, summarizePayload(payload));
                reject(error);
            }

            try {
                socket = new WebSocket(dataSyncWebSocketUrl());
            } catch (error) {
                finishWithError(error);
                return;
            }

            socket.onopen = function () {
                try {
                    debugLog('DataSync websocket opened', summarizePayload(payload));
                    socket.send(JSON.stringify(payload));
                } catch (error) {
                    finishWithError(error);
                }
            };

            socket.onmessage = function (event) {
                if (settled) {
                    return;
                }
                settled = true;
                clearTimeout(timeout);
                try {
                    if (socket && socket.readyState === WebSocket.OPEN) {
                        socket.close();
                    }
                } catch (_error) {}
                try {
                    var decoded = JSON.parse(event.data);
                    debugLog('Received DataSync websocket response', decoded);
                    resolve(decoded);
                } catch (error) {
                    reject(error);
                }
            };

            socket.onerror = function () {
                finishWithError(new Error('Failed to connect to DataSync websocket'));
            };

            socket.onclose = function () {
                if (!settled) {
                    finishWithError(new Error('DataSync websocket closed before replay finished'));
                }
            };
        });
    }

    function rewriteQueuedTransactionIds(queue, localTransactionId, remoteTransactionId) {
        if (!localTransactionId || !remoteTransactionId || !Array.isArray(queue)) {
            return;
        }
        for (var i = 0; i < queue.length; i += 1) {
            var payload = queue[i] && queue[i].payload;
            if (!payload) {
                continue;
            }
            if (payload.transactionId === localTransactionId) {
                payload.transactionId = remoteTransactionId;
            }
            if ((payload.tag === 'CommitTransaction' || payload.tag === 'RollbackTransaction') && payload.id === localTransactionId) {
                payload.id = remoteTransactionId;
            }
        }
    }

    function normalizeRoutePath(routePath) {
        if (!routePath) {
            return '/';
        }
        try {
            return new URL(String(routePath), window.location.origin).pathname;
        } catch (_error) {
            var fallback = String(routePath).split('?')[0];
            if (!fallback) {
                return '/';
            }
            return fallback.charAt(0) === '/' ? fallback : '/' + fallback;
        }
    }

    function createFormData(form, submitter) {
        if (submitter !== undefined && submitter !== null) {
            try {
                return new FormData(form, submitter);
            } catch (_error) {
                return new FormData(form);
            }
        }
        return new FormData(form);
    }

    function formDataToObject(formData) {
        var result = {};
        formData.forEach(function (value, key) {
            if (Object.prototype.hasOwnProperty.call(result, key)) {
                if (!Array.isArray(result[key])) {
                    result[key] = [result[key]];
                }
                result[key].push(value);
            } else {
                result[key] = value;
            }
        });
        return result;
    }

    function readSubmissionMethod(form, formData) {
        var method = (form.getAttribute('method') || 'GET').toUpperCase();
        var methodOverride = formData.get('_method');
        if (typeof methodOverride === 'string' && methodOverride.length > 0) {
            method = methodOverride.toUpperCase();
        }
        return method;
    }

    function readSubmissionPath(form) {
        var actionAttribute = form.getAttribute('action');
        var action = (actionAttribute && actionAttribute.length > 0) ? actionAttribute : window.location.pathname;
        return normalizeRoutePath(action);
    }

    function readSubmissionReplayPath(form) {
        var actionAttribute = form.getAttribute('action');
        var action = (actionAttribute && actionAttribute.length > 0)
            ? actionAttribute
            : (window.location.pathname + window.location.search);
        try {
            var parsed = new URL(String(action), window.location.origin);
            return parsed.pathname + parsed.search;
        } catch (_error) {
            return String(action);
        }
    }

    function normalizeDomSnapshotFieldType(fieldType) {
        if (fieldType === 'bool' || fieldType === 'boolean') {
            return 'bool';
        }
        return 'text';
    }

    function formMatchesActionPath(form, actionPath) {
        if (!form || typeof form.getAttribute !== 'function') {
            return false;
        }
        var actionAttribute = form.getAttribute('action');
        var action = (actionAttribute && actionAttribute.length > 0) ? actionAttribute : window.location.pathname;
        return normalizeRoutePath(action) === actionPath;
    }

    function readSnapshotFieldValue(form, formData, fieldName, fieldType) {
        var normalizedFieldType = normalizeDomSnapshotFieldType(fieldType);
        if (normalizedFieldType === 'bool') {
            var rawBool = formData.get(fieldName);
            if (rawBool === true || rawBool === 'true' || rawBool === 'on' || rawBool === '1') {
                return true;
            }
            if (rawBool === false || rawBool === 'false' || rawBool === '0') {
                return false;
            }

            if (form && form.elements) {
                var element = null;
                if (typeof form.elements.namedItem === 'function') {
                    element = form.elements.namedItem(fieldName);
                } else {
                    element = form.elements[fieldName];
                }
                if (element && typeof element.checked === 'boolean') {
                    return element.checked;
                }
                if (element && typeof element.length === 'number' && element.length > 0) {
                    for (var index = 0; index < element.length; index += 1) {
                        if (element[index] && typeof element[index].checked === 'boolean' && element[index].checked) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        var rawText = formData.get(fieldName);
        if (rawText === null || rawText === undefined) {
            return '';
        }
        return String(rawText);
    }

    function collectDomSnapshotRecords(actionPath, descriptor) {
        var forms = Array.prototype.slice.call(document.querySelectorAll('form'))
            .filter(function (form) {
                return formMatchesActionPath(form, actionPath);
            });
        var fields = withArray(descriptor && descriptor.fields);
        var idField = descriptor && descriptor.idField ? String(descriptor.idField) : null;
        if (!idField) {
            return { hasMatchingForms: forms.length > 0, records: [] };
        }

        var records = forms
            .map(function (form) {
                var formData = createFormData(form, null);
                var id = formData.get(idField);
                if (id === null || id === undefined || String(id).length === 0) {
                    return null;
                }
                var record = { id: String(id) };
                fields.forEach(function (field) {
                    if (!field || !field.column || !field.formField) {
                        return;
                    }
                    record[String(field.column)] = readSnapshotFieldValue(
                        form,
                        formData,
                        String(field.formField),
                        field.fieldType
                    );
                });
                return record;
            })
            .filter(function (record) {
                return !!record;
            });

        return { hasMatchingForms: forms.length > 0, records: records };
    }

    function normalizeActionMethods(methods) {
        var defaultMethods = ['POST', 'PUT', 'PATCH', 'DELETE'];
        var source = Array.isArray(methods) && methods.length > 0 ? methods : defaultMethods;
        return source.map(function (method) {
            return String(method).toUpperCase();
        });
    }

    function appendFormValue(urlParams, key, value) {
        if (value === null || value === undefined) {
            return;
        }
        if (Array.isArray(value)) {
            value.forEach(function (entry) {
                appendFormValue(urlParams, key, entry);
            });
            return;
        }
        urlParams.append(key, String(value));
    }

    async function replayQueuedAction(item) {
        if (!item || !item.routePath) {
            debugWarn('Skipping replay action without routePath', item);
            return;
        }
        debugLog('Replaying queued form action', {
            routePath: item.routePath,
            method: item.method,
            formFieldKeys: Object.keys(item.formFields || {}),
        });
        var originalMethod = String(item.method || 'POST').toUpperCase();
        var requestMethod = originalMethod;
        var urlParams = new URLSearchParams();
        var formFields = item.formFields || {};
        Object.keys(formFields).forEach(function (key) {
            appendFormValue(urlParams, key, formFields[key]);
        });

        if (requestMethod !== 'GET' && requestMethod !== 'POST') {
            urlParams.append('_method', requestMethod);
            requestMethod = 'POST';
        }

        var response = await fetch(item.routePath, {
            method: requestMethod,
            headers: {
                'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
                'X-Requested-With': 'XMLHttpRequest',
            },
            body: requestMethod === 'GET' ? undefined : urlParams.toString(),
            credentials: 'same-origin',
        });
        if (!response.ok) {
            debugWarn('Queued action replay failed with non-OK HTTP status', {
                routePath: item.routePath,
                method: requestMethod,
                status: response.status,
            });
            throw new Error('Replay action request failed with status ' + String(response.status));
        }
        debugLog('Queued action replay succeeded', {
            routePath: item.routePath,
            method: requestMethod,
            status: response.status,
        });
    }

    function resolveConnectivityProbePath(metadata) {
        if (metadata && metadata.reconnectProbePath) {
            return metadata.reconnectProbePath;
        }
        if (typeof window !== 'undefined' && window.location) {
            return window.location.pathname || '/';
        }
        return '/';
    }

    async function probeConnectivity(path, timeoutMs) {
        debugLog('Probing connectivity', { path: path, timeoutMs: timeoutMs });
        var controller = null;
        var timeoutId = null;
        if (typeof AbortController !== 'undefined') {
            controller = new AbortController();
            timeoutId = setTimeout(function () {
                try {
                    controller.abort();
                } catch (_error) {}
            }, timeoutMs);
        }

        try {
            var response = await fetch(path, {
                method: 'GET',
                cache: 'no-store',
                credentials: 'same-origin',
                headers: { 'X-IHP-Local-Probe': '1' },
                signal: controller ? controller.signal : undefined,
            });
            var isReachable = response.ok || (response.status >= 300 && response.status < 400);
            debugLog('Connectivity probe finished', {
                path: path,
                status: response.status,
                reachable: isReachable,
            });
            return isReachable;
        } catch (error) {
            debugWarn('Connectivity probe failed', {
                path: path,
                timeoutMs: timeoutMs,
                error: String(error && error.message ? error.message : error),
            });
            return false;
        } finally {
            if (timeoutId) {
                clearTimeout(timeoutId);
            }
        }
    }

    async function executeInsert(db, payload) {
        var record = decodeDataSyncRecord(payload.record);
        var columns = Object.keys(record || {});
        var sql;
        var params = [];
        if (columns.length === 0) {
            sql = 'INSERT INTO ' + quoteIdentifier(payload.table) + ' DEFAULT VALUES RETURNING *';
        } else {
            var placeholders = columns.map(function () { return '?'; }).join(', ');
            sql = 'INSERT INTO ' + quoteIdentifier(payload.table)
                + ' (' + columns.map(quoteIdentifier).join(', ') + ')'
                + ' VALUES (' + placeholders + ') RETURNING *';
            params = columns.map(function (column) { return record[column]; });
        }
        var result = await db.query(sql, params);
        return { tag: 'DidCreateRecord', requestId: payload.requestId, record: toRows(result)[0] || null };
    }

    async function executeInsertMany(db, payload) {
        var records = withArray(payload.records).map(decodeDataSyncRecord);
        if (records.length === 0) {
            return { tag: 'DidCreateRecords', requestId: payload.requestId, records: [] };
        }
        var columns = Object.keys(records[0]);
        var valuesClause = [];
        var params = [];
        records.forEach(function (record) {
            valuesClause.push('(' + columns.map(function () { return '?'; }).join(', ') + ')');
            columns.forEach(function (column) { params.push(record[column]); });
        });
        var sql = 'INSERT INTO ' + quoteIdentifier(payload.table)
            + ' (' + columns.map(quoteIdentifier).join(', ') + ') VALUES '
            + valuesClause.join(', ')
            + ' RETURNING *';
        var result = await db.query(sql, params);
        return { tag: 'DidCreateRecords', requestId: payload.requestId, records: toRows(result) };
    }

    async function executeUpdate(db, payload) {
        var patch = decodeDataSyncRecord(payload.patch);
        var columns = Object.keys(patch || {});
        if (columns.length === 0) {
            var existing = await db.query(
                'SELECT * FROM ' + quoteIdentifier(payload.table) + ' WHERE id = ?',
                [payload.id]
            );
            return { tag: 'DidUpdateRecord', requestId: payload.requestId, record: toRows(existing)[0] || null };
        }
        var setClause = columns.map(function (column) { return quoteIdentifier(column) + ' = ?'; }).join(', ');
        var params = columns.map(function (column) { return patch[column]; });
        params.push(payload.id);
        var sql = 'UPDATE ' + quoteIdentifier(payload.table)
            + ' SET ' + setClause
            + ' WHERE id = ? RETURNING *';
        var result = await db.query(sql, params);
        return { tag: 'DidUpdateRecord', requestId: payload.requestId, record: toRows(result)[0] || null };
    }

    async function executeUpdateMany(db, payload) {
        var patch = decodeDataSyncRecord(payload.patch);
        var columns = Object.keys(patch || {});
        var ids = withArray(payload.ids);
        if (ids.length === 0) {
            return { tag: 'DidUpdateRecords', requestId: payload.requestId, records: [] };
        }
        if (columns.length === 0) {
            var existing = await db.query(
                'SELECT * FROM ' + quoteIdentifier(payload.table)
                + ' WHERE id IN (' + ids.map(function () { return '?'; }).join(', ') + ')',
                ids
            );
            return { tag: 'DidUpdateRecords', requestId: payload.requestId, records: toRows(existing) };
        }
        var setClause = columns.map(function (column) { return quoteIdentifier(column) + ' = ?'; }).join(', ');
        var inClause = ids.map(function () { return '?'; }).join(', ');
        var params = columns.map(function (column) { return patch[column]; }).concat(ids);
        var sql = 'UPDATE ' + quoteIdentifier(payload.table)
            + ' SET ' + setClause
            + ' WHERE id IN (' + inClause + ') RETURNING *';
        var result = await db.query(sql, params);
        return { tag: 'DidUpdateRecords', requestId: payload.requestId, records: toRows(result) };
    }

    async function executeDelete(db, payload) {
        var sql = 'DELETE FROM ' + quoteIdentifier(payload.table) + ' WHERE id = ?';
        await db.query(sql, [payload.id]);
        return { tag: 'DidDeleteRecord', requestId: payload.requestId };
    }

    async function executeDeleteMany(db, payload) {
        var ids = withArray(payload.ids);
        if (ids.length === 0) {
            return { tag: 'DidDeleteRecords', requestId: payload.requestId };
        }
        var sql = 'DELETE FROM ' + quoteIdentifier(payload.table)
            + ' WHERE id IN (' + ids.map(function () { return '?'; }).join(', ') + ')';
        await db.query(sql, ids);
        return { tag: 'DidDeleteRecords', requestId: payload.requestId };
    }

    function collectRecordColumns(records) {
        var columnsMap = {};
        records.forEach(function (record) {
            Object.keys(record || {}).forEach(function (column) {
                columnsMap[column] = true;
            });
        });
        return Object.keys(columnsMap);
    }

    async function executeUpsertMany(db, table, records) {
        var normalizedRecords = withArray(records)
            .map(decodeDataSyncRecord)
            .filter(function (record) {
                return record && typeof record === 'object';
            });
        if (normalizedRecords.length === 0) {
            return;
        }

        var columns = collectRecordColumns(normalizedRecords);
        if (columns.length === 0) {
            return;
        }

        var valuesClause = [];
        var params = [];
        normalizedRecords.forEach(function (record) {
            valuesClause.push('(' + columns.map(function () { return '?'; }).join(', ') + ')');
            columns.forEach(function (column) {
                if (Object.prototype.hasOwnProperty.call(record, column)) {
                    params.push(record[column]);
                } else {
                    params.push(null);
                }
            });
        });

        var sql = 'INSERT INTO ' + quoteIdentifier(table)
            + ' (' + columns.map(quoteIdentifier).join(', ') + ') VALUES '
            + valuesClause.join(', ');
        if (columns.indexOf('id') !== -1) {
            var updatableColumns = columns.filter(function (column) { return column !== 'id'; });
            if (updatableColumns.length === 0) {
                sql += ' ON CONFLICT (' + quoteIdentifier('id') + ') DO NOTHING';
            } else {
                sql += ' ON CONFLICT (' + quoteIdentifier('id') + ') DO UPDATE SET '
                    + updatableColumns
                        .map(function (column) {
                            return quoteIdentifier(column) + ' = EXCLUDED.' + quoteIdentifier(column);
                        })
                        .join(', ');
            }
        }
        await db.query(sql, params);
    }

    async function readExistingRowsByIds(db, table, ids) {
        var validIds = withArray(ids).filter(function (id) {
            return id !== null && id !== undefined;
        });
        if (validIds.length === 0) {
            return {};
        }
        var result = await db.query(
            'SELECT * FROM ' + quoteIdentifier(table) + ' WHERE id IN (' + validIds.map(function () { return '?'; }).join(', ') + ')',
            validIds
        );
        var byId = {};
        toRows(result).forEach(function (row) {
            if (row && row.id !== undefined && row.id !== null) {
                byId[row.id] = row;
            }
        });
        return byId;
    }

    function queryHasResultWindow(query) {
        return !!query && (typeof query.limit === 'number' || typeof query.offset === 'number');
    }

    async function pruneQuerySnapshot(db, query, records) {
        if (!query || !query.table) {
            return;
        }
        if (queryHasResultWindow(query)) {
            return;
        }

        var whereParameters = [];
        var whereClause = compileCondition(query.whereCondition, whereParameters);
        var ids = withArray(records)
            .map(function (record) {
                if (!record || typeof record !== 'object') {
                    return null;
                }
                return record.id;
            })
            .filter(function (id) {
                return id !== null && id !== undefined;
            });

        if (ids.length === 0) {
            await db.query(
                'DELETE FROM ' + quoteIdentifier(query.table) + ' WHERE ' + whereClause,
                whereParameters
            );
            return;
        }

        var sql = 'DELETE FROM ' + quoteIdentifier(query.table)
            + ' WHERE (' + whereClause + ')'
            + ' AND ' + quoteIdentifier('id') + ' NOT IN (' + ids.map(function () { return '?'; }).join(', ') + ')';
        await db.query(sql, whereParameters.concat(ids));
    }

    function parseComparableTimestamp(value) {
        if (value === null || value === undefined) {
            return null;
        }
        if (typeof value === 'number' && Number.isFinite(value)) {
            return value;
        }
        if (value instanceof Date) {
            var dateMillis = value.getTime();
            return Number.isFinite(dateMillis) ? dateMillis : null;
        }
        if (typeof value === 'string') {
            var parsedDate = Date.parse(value);
            if (Number.isFinite(parsedDate)) {
                return parsedDate;
            }
            var parsedNumber = Number(value);
            if (Number.isFinite(parsedNumber)) {
                return parsedNumber;
            }
        }
        return null;
    }

    function normalizeConflictPolicy(metadata) {
        if (!metadata || typeof metadata.conflictPolicy !== 'string') {
            return 'server-wins';
        }
        switch (metadata.conflictPolicy) {
            case 'client-wins':
            case 'last-write-wins':
            case 'server-wins':
                return metadata.conflictPolicy;
            default:
                return 'server-wins';
        }
    }

    function recordsAreEqual(left, right) {
        if (left === right) {
            return true;
        }
        if (!left || !right) {
            return false;
        }
        try {
            return JSON.stringify(left) === JSON.stringify(right);
        } catch (_error) {
            return false;
        }
    }

    function emitConflictEvent(table, id, metadata, resolution) {
        document.dispatchEvent(new CustomEvent('ihp:sync:conflict', {
            detail: {
                table: table,
                id: id || null,
                policy: normalizeConflictPolicy(metadata),
                field: metadata && metadata.conflictField ? metadata.conflictField : null,
                resolution: resolution,
            },
        }));
    }

    function resolveRecordConflict(table, localRecord, incomingRecord, metadata) {
        var resolver = LOCAL_CONFLICT_RESOLVERS[table];
        if (typeof resolver === 'function') {
            try {
                var resolvedByHook = resolver({
                    table: table,
                    localRecord: localRecord ? deepClone(localRecord) : null,
                    incomingRecord: incomingRecord ? deepClone(incomingRecord) : null,
                    policy: normalizeConflictPolicy(metadata),
                    field: metadata && metadata.conflictField ? metadata.conflictField : null,
                });
                if (resolvedByHook === null || (resolvedByHook && typeof resolvedByHook === 'object')) {
                    return {
                        record: resolvedByHook,
                        source: 'hook',
                    };
                }
            } catch (error) {
                debugError('Conflict resolver hook failed', error, { table: table });
            }
        }

        var policy = normalizeConflictPolicy(metadata);
        if (policy === 'client-wins') {
            if (localRecord) {
                return {
                    record: localRecord,
                    source: 'client',
                };
            }
            return {
                record: incomingRecord,
                source: 'server',
            };
        }

        if (policy === 'last-write-wins') {
            var field = metadata && metadata.conflictField ? metadata.conflictField : 'updated_at';
            var localTimestamp = parseComparableTimestamp(localRecord && localRecord[field]);
            var incomingTimestamp = parseComparableTimestamp(incomingRecord && incomingRecord[field]);

            if (localRecord && localTimestamp !== null && incomingTimestamp !== null) {
                if (localTimestamp > incomingTimestamp) {
                    return {
                        record: localRecord,
                        source: 'client',
                    };
                }
                return {
                    record: incomingRecord,
                    source: 'server',
                };
            }
            if (localRecord && localTimestamp !== null && incomingTimestamp === null) {
                return {
                    record: localRecord,
                    source: 'client',
                };
            }
            if (incomingTimestamp !== null) {
                return {
                    record: incomingRecord,
                    source: 'server',
                };
            }
            return {
                record: incomingRecord,
                source: 'server',
            };
        }

        return {
            record: incomingRecord,
            source: 'server',
        };
    }

    function makeErrorResponse(payload, error) {
        return {
            tag: 'DataSyncError',
            requestId: payload.requestId,
            errorMessage: error && error.message ? error.message : String(error),
        };
    }

    function isPGliteUnavailableError(error) {
        if (!error) {
            return false;
        }
        if (error.code === 'IHP_LOCAL_PGLITE_UNAVAILABLE') {
            return true;
        }
        var message = error && error.message ? String(error.message) : String(error);
        return message.indexOf('PGlite is required for local-first runtime') !== -1;
    }

    function isTransientReplayConnectionError(errorMessage) {
        var message = String(errorMessage || '');
        return message.indexOf('Failed to connect to DataSync websocket') !== -1
            || message.indexOf('DataSync websocket closed before replay finished') !== -1
            || message.indexOf('Timed out while replaying local mutation') !== -1;
    }

    function throwIfDataSyncError(response, fallbackMessage) {
        if (response && response.tag === 'DataSyncError') {
            throw new Error(response.errorMessage || fallbackMessage || 'Local DataSync operation failed');
        }
    }

    var LocalRuntime = {
        db: null,
        dbInitPromise: null,
        schemaBootstrapped: false,
        bootstrappedSchemaSql: null,
        activeTransactions: {},
        remoteDispatcher: null,
        replayInFlight: null,
        replayRequestIdCounter: 1,
        suppressMutationQueue: false,
        connectivityProbeTimer: null,
        syncState: 'idle',
        initialized: false,

        isBrowserOnline: function () {
            return typeof navigator === 'undefined' ? true : navigator.onLine !== false;
        },

        isOnline: function () {
            return this.isBrowserOnline();
        },

        setSyncState: function (state, detail) {
            if (this.syncState === state) {
                debugLog('Sync state unchanged', { state: state, detail: detail || null });
                return;
            }
            debugLog('Sync state transition', {
                from: this.syncState,
                to: state,
                detail: detail || null,
                queues: queueLengths(),
            });
            this.syncState = state;
            document.dispatchEvent(new CustomEvent('ihp:sync:state', {
                detail: Object.assign({ state: state }, detail || {}),
            }));
            requestOnPageDebugPanelRefresh();
        },

        getConnectivityProbeConfig: function () {
            var metadata = readLocalRouteMetadata();
            var config = {
                path: resolveConnectivityProbePath(metadata),
                timeoutMs: metadata ? metadata.reconnectProbeTimeoutMs : 2000,
                intervalMs: metadata ? metadata.reconnectProbeIntervalMs : 15000,
            };
            debugLog('Computed connectivity probe config', config);
            return config;
        },

        ensureOnlineWithProbe: async function () {
            debugLog('Checking connectivity state with probe', { browserOnline: this.isBrowserOnline() });
            if (!this.isBrowserOnline()) {
                this.setSyncState('offline', { reason: 'browser-offline' });
                return false;
            }
            var config = this.getConnectivityProbeConfig();
            if (!config.path) {
                this.setSyncState('online', { reason: 'browser-online' });
                return true;
            }
            this.setSyncState('probing', { path: config.path });
            var isReachable = await probeConnectivity(config.path, config.timeoutMs);
            if (isReachable) {
                this.setSyncState('online', { path: config.path });
                debugLog('Connectivity probe reports online', { path: config.path });
                return true;
            }
            this.setSyncState('offline', { reason: 'probe-failed', path: config.path });
            debugWarn('Connectivity probe reports offline', { path: config.path });
            return false;
        },

        restartConnectivityProbes: function () {
            if (this.connectivityProbeTimer) {
                clearInterval(this.connectivityProbeTimer);
                this.connectivityProbeTimer = null;
                debugLog('Cleared existing connectivity probe timer');
            }
            var self = this;
            var config = this.getConnectivityProbeConfig();
            if (!config.intervalMs || config.intervalMs <= 0) {
                debugWarn('Connectivity probe timer disabled due to non-positive interval', config);
                return;
            }
            debugLog('Starting connectivity probe timer', config);
            this.connectivityProbeTimer = setInterval(function () {
                debugLog('Connectivity probe timer tick');
                self.ensureOnlineWithProbe().then(function (online) {
                    if (!online) {
                        debugWarn('Skipping replay because probe reported offline');
                        return;
                    }
                    self.replayQueuedMutations().catch(function (error) {
                        self.setSyncState('error', { error: error && error.message ? error.message : String(error) });
                        debugError('Replay failed after connectivity probe tick', error);
                    });
                }).catch(function (error) {
                    self.setSyncState('error', { error: error && error.message ? error.message : String(error) });
                    debugError('Connectivity probe tick failed', error);
                });
            }, config.intervalMs);
        },

        isReadyForLocal: function () {
            return isLocalRouteActive() && !this.isBrowserOnline();
        },

        registerRenderer: function (routePath, renderFn) {
            LOCAL_RENDERERS[routePath] = renderFn;
            debugLog('Registered local renderer', { routePath: routePath });
        },

        registerAction: function (routePath, actionHandler, options) {
            if (!routePath) {
                throw new Error('routePath is required when registering a local action');
            }
            if (typeof actionHandler !== 'function') {
                throw new Error('actionHandler needs to be a function');
            }
            var normalizedRoutePath = normalizeRoutePath(routePath);
            LOCAL_ACTIONS[normalizedRoutePath] = {
                handler: actionHandler,
                methods: normalizeActionMethods(options && options.methods),
            };
            debugLog('Registered local action', {
                routePath: routePath,
                normalizedRoutePath: normalizedRoutePath,
                methods: LOCAL_ACTIONS[normalizedRoutePath].methods,
            });
        },

        registerDomSnapshot: function (actionPath, descriptor) {
            if (!actionPath) {
                throw new Error('actionPath is required when registering a DOM snapshot');
            }
            if (!descriptor || typeof descriptor !== 'object' || !descriptor.table || !descriptor.idField) {
                throw new Error('registerDomSnapshot expects { table, idField, fields } descriptor');
            }
            var normalizedActionPath = normalizeRoutePath(actionPath);
            LOCAL_DOM_SNAPSHOTS[normalizedActionPath] = {
                table: String(descriptor.table),
                idField: String(descriptor.idField),
                fields: withArray(descriptor.fields).map(function (field) {
                    return {
                        column: field && field.column ? String(field.column) : '',
                        formField: field && field.formField ? String(field.formField) : '',
                        fieldType: normalizeDomSnapshotFieldType(field && field.fieldType),
                    };
                }),
            };
            debugLog('Registered DOM snapshot descriptor', {
                actionPath: actionPath,
                normalizedActionPath: normalizedActionPath,
                descriptor: LOCAL_DOM_SNAPSHOTS[normalizedActionPath],
            });
        },

        unregisterDomSnapshot: function (actionPath) {
            if (!actionPath) {
                return;
            }
            delete LOCAL_DOM_SNAPSHOTS[normalizeRoutePath(actionPath)];
            debugLog('Unregistered DOM snapshot descriptor', { actionPath: actionPath });
        },

        syncDomSnapshots: async function () {
            if (!isLocalRouteActive() || !this.isBrowserOnline()) {
                debugLog('Skipping DOM snapshot sync because local route is inactive or browser is offline', {
                    localRouteActive: isLocalRouteActive(),
                    browserOnline: this.isBrowserOnline(),
                });
                return;
            }
            if (typeof window === 'undefined' || !window.PGlite) {
                debugWarn('Skipping DOM snapshot sync because PGlite is unavailable on window');
                return;
            }
            var actionPaths = Object.keys(LOCAL_DOM_SNAPSHOTS);
            if (actionPaths.length === 0) {
                debugLog('Skipping DOM snapshot sync because no descriptors are registered');
                return;
            }
            debugLog('Syncing DOM snapshot descriptors', { actionPaths: actionPaths });

            for (var index = 0; index < actionPaths.length; index += 1) {
                var actionPath = actionPaths[index];
                var descriptor = LOCAL_DOM_SNAPSHOTS[actionPath];
                if (!descriptor || !descriptor.table) {
                    debugWarn('Skipping invalid DOM snapshot descriptor', { actionPath: actionPath, descriptor: descriptor });
                    continue;
                }
                var snapshot = collectDomSnapshotRecords(actionPath, descriptor);
                if (!snapshot.hasMatchingForms) {
                    debugLog('No matching forms for DOM snapshot descriptor', { actionPath: actionPath });
                    continue;
                }
                debugLog('Applying DOM snapshot descriptor', {
                    actionPath: actionPath,
                    table: descriptor.table,
                    recordCount: snapshot.records.length,
                });
                await this.syncDataSubscriptionSnapshot(
                    { table: descriptor.table },
                    snapshot.records
                );
            }
        },

        registerConflictResolver: function (table, resolver) {
            if (!table || typeof resolver !== 'function') {
                throw new Error('registerConflictResolver expects a table name and resolver function');
            }
            LOCAL_CONFLICT_RESOLVERS[String(table)] = resolver;
            debugLog('Registered conflict resolver', { table: String(table) });
        },

        unregisterConflictResolver: function (table) {
            if (!table) {
                return;
            }
            delete LOCAL_CONFLICT_RESOLVERS[String(table)];
            debugLog('Unregistered conflict resolver', { table: String(table) });
        },

        getCurrentMetadata: function () {
            return readLocalRouteMetadata();
        },

        unregisterAction: function (routePath) {
            if (!routePath) {
                return;
            }
            delete LOCAL_ACTIONS[normalizeRoutePath(routePath)];
            debugLog('Unregistered local action', { routePath: routePath });
        },

        registerRemoteDispatcher: function (remoteDispatcher) {
            this.remoteDispatcher = remoteDispatcher;
            debugLog('Registered remote dispatcher', { online: this.isOnline(), queues: queueLengths() });
            if (this.isOnline()) {
                this.replayQueuedMutations().catch(function (error) {
                    debugError('Replay failed right after registering remote dispatcher', error);
                });
            }
        },

        setCurrentUser: function (userId) {
            setCurrentUserId(userId);
            debugLog('setCurrentUser called', { userId: userId });
        },

        clearCurrentUserData: async function () {
            var userId = getCurrentUserId();
            debugLog('Clearing all local-first state for user', { userId: userId, queuesBefore: queueLengths() });
            localStorage.removeItem(LOCAL_QUEUE_KEY_PREFIX + userId);
            localStorage.removeItem(LOCAL_FAILED_QUEUE_KEY_PREFIX + userId);
            localStorage.removeItem(LOCAL_ACTION_QUEUE_KEY_PREFIX + userId);
            localStorage.removeItem(LOCAL_USER_KEY);
            if (this.db && this.db.close) {
                await this.db.close();
                debugLog('Closed local PGlite connection while clearing user data');
            }
            this.db = null;
            debugLog('Cleared local-first user state');
        },

        reloadCurrentPage: function () {
            if (typeof window === 'undefined' || !window.location) {
                debugWarn('Cannot reload page because window.location is unavailable');
                return;
            }
            debugLog('Reloading current page for reconciliation', { href: window.location.href });
            if (window.Turbolinks && typeof window.Turbolinks.visit === 'function') {
                try {
                    if (typeof window.Turbolinks.clearCache === 'function') {
                        window.Turbolinks.clearCache();
                        debugLog('Cleared Turbolinks cache before reload');
                    }
                } catch (_error) {}
                window.Turbolinks.visit(window.location.href, { action: 'replace' });
                return;
            }
            if (typeof window.location.reload === 'function') {
                window.location.reload();
            }
        },

        bootstrapSchema: async function (schemaSql) {
            this.bootstrappedSchemaSql = schemaSql;
            if (!schemaSql) {
                debugWarn('bootstrapSchema called with empty schema SQL');
                return;
            }
            debugLog('Bootstrapping local schema', {
                schemaLength: schemaSql.length,
                hasDb: !!this.db,
                alreadyBootstrapped: this.schemaBootstrapped,
            });
            var self = this;
            if (this.db && !this.schemaBootstrapped) {
                await this.db.exec(schemaSql);
                this.schemaBootstrapped = true;
                debugLog('Bootstrapped schema directly on existing DB instance');
            }
            if (!this.db) {
                this.ensureDb().then(function () {
                    if (self.isBrowserOnline()) {
                        return self.syncDomSnapshots();
                    }
                    return null;
                }).catch(function (error) {
                    if (!isPGliteUnavailableError(error)) {
                        debugError('Failed to initialize local DB during schema bootstrap', error);
                    }
                });
            }
            if (this.isBrowserOnline()) {
                this.syncDomSnapshots().catch(function (error) {
                    if (!isPGliteUnavailableError(error)) {
                        debugError('Failed to sync DOM snapshots right after bootstrapSchema', error);
                    }
                });
            }
        },

        ensureDb: async function () {
            if (this.db) {
                debugLog('Returning cached local DB handle');
                return this.db;
            }
            if (this.dbInitPromise) {
                debugLog('Awaiting in-flight local DB initialization');
                return this.dbInitPromise;
            }

            var self = this;
            this.dbInitPromise = (async function () {
                debugLog('Starting local DB initialization');
                var pgliteCtor = window.PGlite || await waitForPGlite(5000);
                if (!pgliteCtor) {
                    var pgliteUnavailableError = new Error('PGlite is required for local-first runtime but window.PGlite is not available');
                    pgliteUnavailableError.code = 'IHP_LOCAL_PGLITE_UNAVAILABLE';
                    debugWarn('Local DB initialization failed because PGlite is unavailable');
                    throw pgliteUnavailableError;
                }
                var filesystems = pickFilesystemCandidates();
                var lastError = null;
                for (var index = 0; index < filesystems.length; index += 1) {
                    try {
                        debugLog('Attempting local DB initialization on filesystem candidate', { dataDir: filesystems[index] });
                        self.db = await openPGlite(filesystems[index], pgliteCtor);
                        debugLog('Initialized local DB on filesystem candidate', { dataDir: filesystems[index] });
                        break;
                    } catch (error) {
                        lastError = error;
                        debugWarn('Local DB candidate initialization failed', {
                            dataDir: filesystems[index],
                            error: String(error && error.message ? error.message : error),
                        });
                    }
                }
                if (!self.db) {
                    debugError('Unable to initialize local PGlite database', lastError);
                    throw (lastError || new Error('Unable to initialize local PGlite database'));
                }
                if (self.db && !self.db.__ihpPlaceholderCompat && typeof self.db.query === "function") {
                    var originalQuery = self.db.query.bind(self.db);
                    self.db.query = function (sql, params) {
                        if (typeof sql === "string" && sql.indexOf("?") !== -1 && Array.isArray(params) && params.length > 0) {
                            var position = 0;
                            sql = sql.replace(/\?/g, function () {
                                position += 1;
                                return "$" + String(position);
                            });
                        }
                        return originalQuery(sql, params);
                    };
                    self.db.__ihpPlaceholderCompat = true;
                    debugLog('Installed placeholder compatibility wrapper for local DB driver');
                }
                if (self.bootstrappedSchemaSql && !self.schemaBootstrapped) {
                    await self.db.exec(self.bootstrappedSchemaSql);
                    self.schemaBootstrapped = true;
                    debugLog('Bootstrapped schema during local DB initialization');
                }
                return self.db;
            })();

            try {
                var db = await this.dbInitPromise;
                debugLog('Local DB initialization complete');
                return db;
            } finally {
                this.dbInitPromise = null;
            }
        },

        refreshActiveLocalRoute: async function () {
            var metadata = readLocalRouteMetadata();
            if (!metadata || !metadata.routePath) {
                debugLog('Skipping local route refresh because no active route metadata exists');
                return;
            }
            var render = LOCAL_RENDERERS[metadata.routePath];
            if (!render) {
                debugLog('Refreshing local route without renderer (event only)', { routePath: metadata.routePath });
                document.dispatchEvent(new CustomEvent('ihp:local-refresh', { detail: { routePath: metadata.routePath } }));
                return;
            }
            debugLog('Refreshing local route via registered renderer', { routePath: metadata.routePath });
            var html = await render();
            if (typeof html === 'string' && html.length > 0) {
                debugLog('Renderer produced HTML for local refresh', { routePath: metadata.routePath, htmlLength: html.length });
                document.dispatchEvent(new CustomEvent('ihp:local-refresh', { detail: { routePath: metadata.routePath, html: html } }));
            }
        },

        shouldHandleFormLocally: function (routePath, method) {
            var readyForLocal = this.isReadyForLocal();
            if (!readyForLocal) {
                debugLog('Form will not be handled locally because runtime is not ready for local mode', {
                    routePath: routePath,
                    method: method,
                    readyForLocal: readyForLocal,
                    browserOnline: this.isBrowserOnline(),
                });
                return false;
            }
            var action = LOCAL_ACTIONS[normalizeRoutePath(routePath)];
            if (!action) {
                debugLog('Form will not be handled locally because no local action is registered', {
                    routePath: routePath,
                    method: method,
                });
                return false;
            }
            var matchesMethod = action.methods.indexOf(String(method).toUpperCase()) !== -1;
            debugLog('Evaluated local form handling decision', {
                routePath: routePath,
                method: method,
                matchesMethod: matchesMethod,
                registeredMethods: action.methods,
            });
            return matchesMethod;
        },

        executeRegisteredAction: async function (routePath, request) {
            var normalizedRoutePath = normalizeRoutePath(routePath);
            var action = LOCAL_ACTIONS[normalizedRoutePath];
            if (!action) {
                throw new Error('No local action registered for route: ' + normalizedRoutePath);
            }
            debugLog('Executing registered local action', {
                routePath: routePath,
                normalizedRoutePath: normalizedRoutePath,
                method: request && request.method ? request.method : null,
            });
            return await action.handler(request);
        },

        handleOfflineFormSubmission: async function (form, submitter) {
            var formData = createFormData(form, submitter);
            var method = readSubmissionMethod(form, formData);
            var routePath = readSubmissionPath(form);
            var replayPath = readSubmissionReplayPath(form);
            debugLog('Handling offline form submission', {
                routePath: routePath,
                replayPath: replayPath,
                method: method,
            });
            if (!this.shouldHandleFormLocally(routePath, method)) {
                debugLog('Offline form submission is not handled locally', {
                    routePath: routePath,
                    method: method,
                });
                return false;
            }
            var formFields = formDataToObject(formData);
            debugLog('Offline form submission payload', {
                routePath: routePath,
                method: method,
                formFields: formFields,
            });
            try {
                var previousSuppressMutationQueue = this.suppressMutationQueue;
                this.suppressMutationQueue = true;
                var result = null;
                try {
                    result = await this.executeRegisteredAction(routePath, {
                        routePath: routePath,
                        method: method,
                        form: form,
                        submitter: submitter || null,
                        formData: formData,
                        formFields: formFields,
                    });
                } finally {
                    this.suppressMutationQueue = previousSuppressMutationQueue;
                }
                appendQueuedAction({
                    routePath: replayPath,
                    method: method,
                    formFields: deepClone(formFields),
                    createdAt: new Date().toISOString(),
                });
                debugLog('Offline form action handled locally and queued for replay', {
                    routePath: routePath,
                    replayPath: replayPath,
                    method: method,
                    queues: queueLengths(),
                });
                document.dispatchEvent(new CustomEvent('ihp:local-action:success', {
                    detail: { routePath: routePath, method: method, result: result },
                }));
                return true;
            } catch (error) {
                document.dispatchEvent(new CustomEvent('ihp:local-action:error', {
                    detail: { routePath: routePath, method: method, error: error },
                }));
                debugError('Local action failed during offline form submission', error, {
                    routePath: routePath,
                    method: method,
                    formFields: formFields,
                });
                return true;
            }
        },

        installOfflineActionFormInterceptor: function () {
            if (localActionFormInterceptorInstalled) {
                debugLog('Offline action form interceptor already installed');
                return;
            }
            localActionFormInterceptorInstalled = true;
            debugLog('Installing offline action form interceptor');
            var self = this;
            document.addEventListener('submit', function (event) {
                var form = event.target;
                if (!form || form.tagName !== 'FORM') {
                    return;
                }
                var submitter = event.submitter || null;
                var formData = createFormData(form, submitter);
                var method = readSubmissionMethod(form, formData);
                var routePath = readSubmissionPath(form);
                if (!self.shouldHandleFormLocally(routePath, method)) {
                    return;
                }
                debugLog('Intercepting form submission for local-first handling', {
                    routePath: routePath,
                    method: method,
                });
                event.preventDefault();
                self.handleOfflineFormSubmission(form, submitter).catch(function (error) {
                    debugError('Failed to handle intercepted local form submission', error, {
                        routePath: routePath,
                        method: method,
                    });
                });
            }, true);
        },

        executeLocal: async function (payload) {
            debugLog('Executing local DataSync payload', summarizePayload(payload));
            var db = await this.ensureDb();

            switch (payload.tag) {
                case 'DataSyncQuery': {
                    var compiled = compileSelectQuery(payload.query);
                    var result = await db.query(compiled.sql, compiled.parameters);
                    debugLog('Executed local DataSync query', {
                        table: payload.query ? payload.query.table : null,
                        rowCount: toRows(result).length,
                    });
                    return { tag: 'DataSyncResult', requestId: payload.requestId, result: toRows(result) };
                }
                case 'CreateRecordMessage':
                    return await executeInsert(db, payload);
                case 'CreateRecordsMessage':
                    return await executeInsertMany(db, payload);
                case 'UpdateRecordMessage':
                    return await executeUpdate(db, payload);
                case 'UpdateRecordsMessage':
                    return await executeUpdateMany(db, payload);
                case 'DeleteRecordMessage':
                    return await executeDelete(db, payload);
                case 'DeleteRecordsMessage':
                    return await executeDeleteMany(db, payload);
                case 'StartTransaction': {
                    var transactionId = (typeof crypto !== 'undefined' && crypto.randomUUID)
                        ? crypto.randomUUID()
                        : String(Date.now());
                    await db.exec('BEGIN');
                    this.activeTransactions[transactionId] = true;
                    return { tag: 'DidStartTransaction', requestId: payload.requestId, transactionId: transactionId };
                }
                case 'CommitTransaction': {
                    await db.exec('COMMIT');
                    delete this.activeTransactions[payload.id];
                    return { tag: 'DidCommitTransaction', requestId: payload.requestId, transactionId: payload.id };
                }
                case 'RollbackTransaction': {
                    await db.exec('ROLLBACK');
                    delete this.activeTransactions[payload.id];
                    return { tag: 'DidRollbackTransaction', requestId: payload.requestId, transactionId: payload.id };
                }
                default:
                    debugWarn('Unsupported local DataSync payload received', summarizePayload(payload));
                    throw new Error('Unsupported local DataSync payload: ' + payload.tag);
            }
        },

        isMutationPayload: function (payload) {
            return [
                'CreateRecordMessage',
                'CreateRecordsMessage',
                'UpdateRecordMessage',
                'UpdateRecordsMessage',
                'DeleteRecordMessage',
                'DeleteRecordsMessage',
                'StartTransaction',
                'CommitTransaction',
                'RollbackTransaction',
            ].indexOf(payload.tag) !== -1;
        },

        shouldHandleLocally: function (payload) {
            if (!payload || !payload.tag) {
                return false;
            }
            if (!isLocalRouteActive()) {
                return false;
            }
            if (this.isOnline()) {
                return false;
            }
            return [
                'DataSyncQuery',
                'CreateRecordMessage',
                'CreateRecordsMessage',
                'UpdateRecordMessage',
                'UpdateRecordsMessage',
                'DeleteRecordMessage',
                'DeleteRecordsMessage',
                'StartTransaction',
                'CommitTransaction',
                'RollbackTransaction',
            ].indexOf(payload.tag) !== -1;
        },

        dispatchDataSync: async function (payload) {
            debugLog('Dispatching DataSync payload to local runtime', summarizePayload(payload));
            try {
                var response = await this.executeLocal(payload);
                debugLog('Local DataSync execution completed', {
                    payload: summarizePayload(payload),
                    responseTag: response ? response.tag : null,
                });
                if (this.isMutationPayload(payload)) {
                    if (!this.suppressMutationQueue) {
                        var queueItem = {
                            payload: deepClone(payload),
                            createdAt: new Date().toISOString(),
                        };
                        if (payload.tag === 'StartTransaction' && response && response.transactionId) {
                            queueItem.localTransactionId = response.transactionId;
                        }
                        appendQueuedMutation(queueItem);
                        debugLog('Queued local mutation for replay', {
                            payload: summarizePayload(payload),
                            queues: queueLengths(),
                        });
                    }
                    await this.refreshActiveLocalRoute();
                }
                return response;
            } catch (error) {
                debugError('Local DataSync execution failed', error, summarizePayload(payload));
                return makeErrorResponse(payload, error);
            }
        },

        createRecord: async function (table, record, options) {
            record = ensureRecordId(record);
            debugLog('createRecord called', {
                table: table,
                recordId: record ? record.id : null,
                keys: record ? Object.keys(record) : [],
            });
            var transactionId = options && options.transactionId ? options.transactionId : null;
            var response = await this.dispatchDataSync({
                tag: 'CreateRecordMessage',
                table: table,
                record: record,
                transactionId: transactionId,
            });
            throwIfDataSyncError(response, 'Failed to create local record');
            return response;
        },

        updateRecord: async function (table, id, patch, options) {
            debugLog('updateRecord called', {
                table: table,
                id: id,
                patchKeys: patch ? Object.keys(patch) : [],
            });
            var transactionId = options && options.transactionId ? options.transactionId : null;
            var response = await this.dispatchDataSync({
                tag: 'UpdateRecordMessage',
                table: table,
                id: id,
                patch: patch,
                transactionId: transactionId,
            });
            throwIfDataSyncError(response, 'Failed to update local record');
            return response;
        },

        deleteRecord: async function (table, id, options) {
            debugLog('deleteRecord called', { table: table, id: id });
            var transactionId = options && options.transactionId ? options.transactionId : null;
            var response = await this.dispatchDataSync({
                tag: 'DeleteRecordMessage',
                table: table,
                id: id,
                transactionId: transactionId,
            });
            throwIfDataSyncError(response, 'Failed to delete local record');
            return response;
        },

        shouldMirrorTable: function (table) {
            if (!table) {
                debugWarn('shouldMirrorTable called without table');
                return false;
            }
            var metadata = readLocalRouteMetadata();
            if (!metadata || !Array.isArray(metadata.syncTables) || metadata.syncTables.length === 0) {
                debugLog('Mirroring table because syncTables is empty', { table: table });
                return true;
            }
            var shouldMirror = metadata.syncTables.indexOf(table) !== -1;
            debugLog('Evaluated table mirroring rule', {
                table: table,
                syncTables: metadata.syncTables,
                shouldMirror: shouldMirror,
            });
            return shouldMirror;
        },

        syncDataSubscriptionSnapshot: async function (query, records) {
            if (!query || !query.table) {
                debugWarn('Ignoring subscription snapshot without query.table', { query: query });
                return;
            }
            debugLog('Syncing subscription snapshot', {
                table: query.table,
                incomingRecordCount: withArray(records).length,
            });
            if (!this.shouldMirrorTable(query.table)) {
                debugLog('Skipping subscription snapshot because table is not mirrored', { table: query.table });
                return;
            }
            var metadata = readLocalRouteMetadata();
            var policy = normalizeConflictPolicy(metadata);
            var normalizedRecords = withArray(records)
                .map(decodeDataSyncRecord)
                .filter(function (record) {
                    return record && typeof record === 'object';
                });
            if (
                normalizedRecords.length > 0
                && !normalizedRecords.every(function (record) {
                    return Object.prototype.hasOwnProperty.call(record, 'id');
                })
            ) {
                debugWarn(
                    '[ihp-local-runtime] skipped snapshot sync for table "' + query.table + '" because records are missing ids'
                );
                return;
            }

            try {
                var db = await this.ensureDb();
                var existingById = await readExistingRowsByIds(
                    db,
                    query.table,
                    normalizedRecords.map(function (record) { return record.id; })
                );
                var resolvedRecords = normalizedRecords.map(function (incomingRecord) {
                    var localRecord = Object.prototype.hasOwnProperty.call(existingById, incomingRecord.id)
                        ? existingById[incomingRecord.id]
                        : null;
                    var decision = resolveRecordConflict(query.table, localRecord, incomingRecord, metadata);
                    var resolvedRecord = decision && decision.record ? decision.record : incomingRecord;

                    if (
                        localRecord
                        && !recordsAreEqual(localRecord, incomingRecord)
                        && decision
                        && decision.source !== 'server'
                        && !recordsAreEqual(resolvedRecord, incomingRecord)
                    ) {
                        emitConflictEvent(
                            query.table,
                            incomingRecord.id,
                            metadata,
                            decision.source === 'hook' ? 'custom' : 'local'
                        );
                    }

                    return resolvedRecord;
                });

                await executeUpsertMany(db, query.table, resolvedRecords);
                if (policy === 'server-wins') {
                    await pruneQuerySnapshot(db, query, resolvedRecords);
                }
                debugLog('Subscription snapshot sync complete', {
                    table: query.table,
                    policy: policy,
                    upsertedRecords: resolvedRecords.length,
                });
            } catch (error) {
                if (!isPGliteUnavailableError(error)) {
                    debugError('Failed to sync subscription snapshot', error, {
                        table: query.table,
                        incomingRecordCount: normalizedRecords.length,
                    });
                }
            }
        },

        applyServerSubscriptionMessage: async function (query, message) {
            if (!query || !query.table || !message || !message.tag) {
                debugWarn('Ignoring invalid server subscription message', { query: query, message: message });
                return;
            }
            debugLog('Applying server subscription message', {
                table: query.table,
                tag: message.tag,
                id: message.id || (message.record && message.record.id) || null,
            });
            if (!this.shouldMirrorTable(query.table)) {
                debugLog('Skipping server subscription message because table is not mirrored', {
                    table: query.table,
                    tag: message.tag,
                });
                return;
            }
            var metadata = readLocalRouteMetadata();

            try {
                var db = await this.ensureDb();
                switch (message.tag) {
                    case 'DidInsert': {
                        if (!message.record) {
                            debugWarn('Ignoring DidInsert message without record', message);
                            return;
                        }
                        var incomingInsertRecord = decodeDataSyncRecord(message.record);
                        var existingInsertRows = await readExistingRowsByIds(db, query.table, [incomingInsertRecord.id]);
                        var localInsertRecord = Object.prototype.hasOwnProperty.call(existingInsertRows, incomingInsertRecord.id)
                            ? existingInsertRows[incomingInsertRecord.id]
                            : null;
                        var insertDecision = resolveRecordConflict(query.table, localInsertRecord, incomingInsertRecord, metadata);
                        var resolvedInsertRecord = insertDecision && insertDecision.record ? insertDecision.record : incomingInsertRecord;

                        if (
                            localInsertRecord
                            && !recordsAreEqual(localInsertRecord, incomingInsertRecord)
                            && insertDecision
                            && insertDecision.source !== 'server'
                            && !recordsAreEqual(resolvedInsertRecord, incomingInsertRecord)
                        ) {
                            emitConflictEvent(
                                query.table,
                                incomingInsertRecord.id,
                                metadata,
                                insertDecision.source === 'hook' ? 'custom' : 'local'
                            );
                        }

                        await executeUpsertMany(db, query.table, [resolvedInsertRecord]);
                        debugLog('Applied DidInsert subscription message', {
                            table: query.table,
                            id: resolvedInsertRecord ? resolvedInsertRecord.id : null,
                        });
                        return;
                    }
                    case 'DidUpdate': {
                        if (!message.id) {
                            debugWarn('Ignoring DidUpdate message without id', message);
                            return;
                        }
                        var existingResult = await db.query(
                            'SELECT * FROM ' + quoteIdentifier(query.table) + ' WHERE id = ?',
                            [message.id]
                        );
                        var current = toRows(existingResult)[0] || null;
                        var incomingUpdateRecord = Object.assign({}, current || { id: message.id }, decodeDataSyncRecord(message.changeSet || {}));
                        if (message.appendSet) {
                            Object.keys(message.appendSet).forEach(function (key) {
                                var value = decodeDynamicValue(message.appendSet[key]);
                                var suffix = value === null || value === undefined ? '' : String(value);
                                incomingUpdateRecord[key] = (typeof incomingUpdateRecord[key] === 'string' ? incomingUpdateRecord[key] : '') + suffix;
                            });
                        }
                        var updateDecision = resolveRecordConflict(query.table, current, incomingUpdateRecord, metadata);
                        var resolvedUpdateRecord = updateDecision && updateDecision.record ? updateDecision.record : incomingUpdateRecord;

                        if (
                            current
                            && !recordsAreEqual(current, incomingUpdateRecord)
                            && updateDecision
                            && updateDecision.source !== 'server'
                            && !recordsAreEqual(resolvedUpdateRecord, incomingUpdateRecord)
                        ) {
                            emitConflictEvent(
                                query.table,
                                message.id,
                                metadata,
                                updateDecision.source === 'hook' ? 'custom' : 'local'
                            );
                        }

                        await executeUpsertMany(db, query.table, [resolvedUpdateRecord]);
                        debugLog('Applied DidUpdate subscription message', {
                            table: query.table,
                            id: message.id,
                        });
                        return;
                    }
                    case 'DidDelete': {
                        if (!message.id) {
                            debugWarn('Ignoring DidDelete message without id', message);
                            return;
                        }
                        var existingDeleteResult = await db.query(
                            'SELECT * FROM ' + quoteIdentifier(query.table) + ' WHERE id = ?',
                            [message.id]
                        );
                        var localDeleteRecord = toRows(existingDeleteResult)[0] || null;
                        var deleteDecision = resolveRecordConflict(query.table, localDeleteRecord, null, metadata);
                        if (deleteDecision && deleteDecision.record && typeof deleteDecision.record === 'object') {
                            if (deleteDecision.source !== 'server') {
                                emitConflictEvent(
                                    query.table,
                                    message.id,
                                    metadata,
                                    deleteDecision.source === 'hook' ? 'custom' : 'local'
                                );
                            }
                            await executeUpsertMany(db, query.table, [deleteDecision.record]);
                            debugLog('Resolved DidDelete conflict by keeping local/custom record', {
                                table: query.table,
                                id: message.id,
                            });
                            return;
                        }
                        await db.query(
                            'DELETE FROM ' + quoteIdentifier(query.table) + ' WHERE id = ?',
                            [message.id]
                        );
                        debugLog('Applied DidDelete subscription message', {
                            table: query.table,
                            id: message.id,
                        });
                        return;
                    }
                    default:
                        debugLog('Ignoring unknown server subscription message tag', { tag: message.tag });
                        return;
                }
            } catch (error) {
                debugError('Failed to apply server subscription message', error, {
                    table: query.table,
                    tag: message.tag,
                });
            }
        },

        replayQueuedActions: async function () {
            if (!this.isOnline()) {
                debugLog('Skipping action replay because runtime is offline');
                return { replayedActions: 0, hasPendingActions: false };
            }
            var queuedActions = readActionQueue();
            if (!Array.isArray(queuedActions) || queuedActions.length === 0) {
                debugLog('No queued actions to replay');
                return { replayedActions: 0, hasPendingActions: false };
            }
            debugLog('Starting queued action replay', { queuedActionCount: queuedActions.length });

            var replayedActions = 0;
            var remaining = deepClone(queuedActions);
            for (var index = 0; index < queuedActions.length; index += 1) {
                var item = remaining[0];
                try {
                    await replayQueuedAction(item);
                    replayedActions += 1;
                    remaining.shift();
                } catch (error) {
                    var replayError = makeErrorResponse(
                        { requestId: item && item.routePath ? item.routePath : 'action-replay' },
                        error
                    );
                    appendFailedMutation({
                        payload: item,
                        response: replayError,
                        failedAt: new Date().toISOString(),
                    });
                    this.setSyncState('error', { error: replayError.errorMessage || 'replay action failed' });
                    debugError('Queued action replay failed', replayError, {
                        routePath: item ? item.routePath : null,
                        method: item ? item.method : null,
                    });
                    break;
                }
            }
            writeActionQueue(remaining);
            debugLog('Queued action replay finished', {
                replayedActions: replayedActions,
                remainingActions: remaining.length,
            });
            return {
                replayedActions: replayedActions,
                hasPendingActions: remaining.length > 0,
            };
        },

        replayQueuedMutations: async function () {
            if (this.replayInFlight) {
                debugLog('Awaiting in-flight mutation replay');
                return await this.replayInFlight;
            }

            var self = this;
            this.replayInFlight = (async function () {
                debugLog('Starting mutation replay run', { queues: queueLengths() });
                var isReachable = await self.ensureOnlineWithProbe();
                if (!isReachable) {
                    debugWarn('Stopping mutation replay because connectivity probe reported offline');
                    return;
                }
                self.setSyncState('syncing');
                var replaySummary = await self.replayQueuedActions();
                var shouldReloadAfterActionReplay =
                    replaySummary
                    && replaySummary.replayedActions > 0
                    && !replaySummary.hasPendingActions;
                var replayDispatcher = self.remoteDispatcher || sendDataSyncMessageOverWebSocket;
                var queued = readQueue();
                if (!Array.isArray(queued) || queued.length === 0) {
                    self.setSyncState('online');
                    debugLog('No queued mutations to replay', {
                        replaySummary: replaySummary,
                        queues: queueLengths(),
                    });
                    if (shouldReloadAfterActionReplay) {
                        self.reloadCurrentPage();
                        return;
                    }
                    try {
                        await self.syncDomSnapshots();
                    } catch (error) {
                        debugError('Failed to sync DOM snapshots after empty mutation replay queue', error);
                    }
                    return;
                }
                debugLog('Replaying queued mutations', { queuedMutationCount: queued.length });
                var remaining = deepClone(queued);
                var transactionIdMap = {};
                for (var i = 0; i < queued.length; i += 1) {
                    var item = remaining[0];
                    try {
                        var replayPayload = deepClone(item.payload);
                        if (typeof replayPayload.requestId !== 'number') {
                            replayPayload.requestId = self.replayRequestIdCounter;
                            self.replayRequestIdCounter += 1;
                        }
                        if (replayPayload.transactionId) {
                            var mappedTransactionId = transactionIdMap[replayPayload.transactionId] || replayPayload.transactionId;
                            replayPayload.transactionId = mappedTransactionId;
                        }
                        if ((replayPayload.tag === 'CommitTransaction' || replayPayload.tag === 'RollbackTransaction') && replayPayload.id) {
                            var mappedId = transactionIdMap[replayPayload.id] || replayPayload.id;
                            replayPayload.id = mappedId;
                        }

                        var response = await replayDispatcher(replayPayload);
                        debugLog('Received replay response for mutation', {
                            payload: summarizePayload(replayPayload),
                            responseTag: response ? response.tag : null,
                        });
                        if (replayPayload.tag === 'StartTransaction' && item.localTransactionId && response && response.transactionId) {
                            transactionIdMap[item.localTransactionId] = response.transactionId;
                            rewriteQueuedTransactionIds(remaining, item.localTransactionId, response.transactionId);
                            debugLog('Mapped local transaction id to remote transaction id', {
                                localTransactionId: item.localTransactionId,
                                remoteTransactionId: response.transactionId,
                            });
                        }
                        if (response && response.tag === 'DataSyncError') {
                            if (isTransientReplayConnectionError(response.errorMessage)) {
                                self.setSyncState('offline', { reason: 'datasync-unreachable' });
                                debugWarn('Mutation replay stopped due to transient DataSync connection error', {
                                    message: response.errorMessage,
                                    payload: summarizePayload(replayPayload),
                                });
                                break;
                            }
                            appendFailedMutation({
                                payload: item.payload,
                                response: response,
                                failedAt: new Date().toISOString(),
                            });
                            self.setSyncState('error', { error: response.errorMessage || 'replay mutation failed' });
                            debugError('Mutation replay failed with DataSyncError response', response, {
                                payload: summarizePayload(replayPayload),
                            });
                            break;
                        }
                        remaining.shift();
                    } catch (error) {
                        var replayErrorResponse = makeErrorResponse(item.payload, error);
                        if (isTransientReplayConnectionError(replayErrorResponse.errorMessage)) {
                            self.setSyncState('offline', { reason: 'datasync-unreachable' });
                            debugWarn('Mutation replay stopped due to transient transport error', {
                                message: replayErrorResponse.errorMessage,
                                payload: item ? summarizePayload(item.payload) : null,
                            });
                            break;
                        }
                        appendFailedMutation({
                            payload: item.payload,
                            response: replayErrorResponse,
                            failedAt: new Date().toISOString(),
                        });
                        self.setSyncState('error', { error: replayErrorResponse.errorMessage || 'replay mutation failed' });
                        debugError('Mutation replay failed with exception', replayErrorResponse, {
                            payload: item ? summarizePayload(item.payload) : null,
                        });
                        break;
                    }
                }
                writeQueue(remaining);
                if (remaining.length === 0 && readActionQueue().length === 0) {
                    self.setSyncState('online');
                }
                debugLog('Mutation replay loop finished', {
                    remainingMutations: remaining.length,
                    queues: queueLengths(),
                });
                if (shouldReloadAfterActionReplay && remaining.length === 0 && readActionQueue().length === 0) {
                    self.reloadCurrentPage();
                    return;
                }
                try {
                    await self.syncDomSnapshots();
                } catch (error) {
                    debugError('Failed to sync DOM snapshots after mutation replay', error);
                }
            })();

            try {
                return await this.replayInFlight;
            } finally {
                self.replayInFlight = null;
                debugLog('Mutation replay run finalized', { queues: queueLengths() });
            }
        },

        getFailedMutations: function () {
            var failed = readFailedQueue();
            debugLog('Reading failed mutation queue', { failedCount: failed.length });
            return failed;
        },

        setDebugLogging: function (enabled) {
            try {
                localStorage.setItem(LOCAL_DEBUG_FLAG_KEY, enabled ? '1' : '0');
            } catch (_error) {}
            debugLog('Toggled local-first debug logging', { enabled: enabled });
            requestOnPageDebugPanelRefresh();
        },

        isDebugLoggingEnabled: function () {
            return isLocalDebugEnabled();
        },

        getDebugSnapshot: function () {
            var snapshot = collectRuntimeSnapshot(this);
            debugLog('Built local-first debug snapshot', snapshot);
            return snapshot;
        },

        setDebugPanelVisible: function (enabled) {
            setOnPageDebugPanelEnabled(!!enabled);
            if (enabled) {
                mountOnPageDebugPanel(this);
            } else {
                removeOnPageDebugPanel();
            }
            debugLog('Toggled on-page debug panel visibility', { enabled: !!enabled });
        },

        isDebugPanelVisible: function () {
            return isOnPageDebugPanelEnabled();
        },

        initialize: function () {
            if (this.initialized) {
                debugLog('Local runtime initialize() called more than once; ignoring');
                return;
            }
            this.initialized = true;
            debugLog('Initializing local-first runtime', {
                debugLoggingEnabled: isLocalDebugEnabled(),
                queues: queueLengths(),
            });
            var self = this;
            window.addEventListener('offline', function () {
                debugLog('Received browser offline event');
                self.setSyncState('offline', { reason: 'browser-offline' });
            });
            window.addEventListener('online', function () {
                debugLog('Received browser online event');
                self.replayQueuedMutations().catch(function (error) {
                    self.setSyncState('error', { error: error && error.message ? error.message : String(error) });
                    debugError('Replay failed after browser online event', error);
                });
            });
            window.addEventListener('turbolinks:load', function () {
                debugLog('Received turbolinks:load event');
                self.restartConnectivityProbes();
                mountOnPageDebugPanel(self);
                self.syncDomSnapshots().catch(function (error) {
                    debugError('Failed to sync DOM snapshots on turbolinks:load', error);
                });
            });
            document.addEventListener('ihp:local-debug:refresh', function () {
                refreshOnPageDebugPanel(self);
            });
            if (document.readyState === 'loading') {
                document.addEventListener('DOMContentLoaded', function () {
                    mountOnPageDebugPanel(self);
                });
            } else {
                mountOnPageDebugPanel(self);
            }
            self.restartConnectivityProbes();
            if (self.isBrowserOnline()) {
                debugLog('Browser starts online, scheduling replay');
                self.replayQueuedMutations().catch(function (error) {
                    self.setSyncState('error', { error: error && error.message ? error.message : String(error) });
                    debugError('Replay failed during initial online bootstrap', error);
                });
            } else {
                debugLog('Browser starts offline');
                self.setSyncState('offline', { reason: 'browser-offline' });
            }
        },
    };

    LocalRuntime.initialize();
    window.IHPLocalRuntime = LocalRuntime;
})();
