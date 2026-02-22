(function () {
    var LOCAL_USER_KEY = 'ihp_local_user_id';
    var LOCAL_QUEUE_KEY_PREFIX = 'ihp_local_queue:';
    var LOCAL_FAILED_QUEUE_KEY_PREFIX = 'ihp_local_failed_queue:';
    var LOCAL_ACTION_QUEUE_KEY_PREFIX = 'ihp_local_action_queue:';
    var LOCAL_DB_PREFIX = 'idb://ihp-local-db:';
    var LOCAL_RENDERERS = {};
    var LOCAL_ACTIONS = {};
    var LOCAL_CONFLICT_RESOLVERS = {};
    var localActionFormInterceptorInstalled = false;

    function parseJson(value, fallback) {
        try {
            return JSON.parse(value);
        } catch (_error) {
            return fallback;
        }
    }

    function getCurrentUserId() {
        return localStorage.getItem(LOCAL_USER_KEY) || 'anonymous';
    }

    function setCurrentUserId(userId) {
        if (!userId) {
            localStorage.removeItem(LOCAL_USER_KEY);
            return;
        }
        localStorage.setItem(LOCAL_USER_KEY, userId);
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
        localStorage.setItem(queueKey(), JSON.stringify(queue));
    }

    function readFailedQueue() {
        return parseJson(localStorage.getItem(failedQueueKey()) || '[]', []);
    }

    function writeFailedQueue(queue) {
        localStorage.setItem(failedQueueKey(), JSON.stringify(queue));
    }

    function readActionQueue() {
        return parseJson(localStorage.getItem(actionQueueKey()) || '[]', []);
    }

    function writeActionQueue(queue) {
        localStorage.setItem(actionQueueKey(), JSON.stringify(queue));
    }

    function appendFailedMutation(item) {
        var queue = readFailedQueue();
        queue.push(item);
        writeFailedQueue(queue);
    }

    function appendQueuedMutation(item) {
        var queue = readQueue();
        queue.push(item);
        writeQueue(queue);
    }

    function appendQueuedAction(item) {
        var queue = readActionQueue();
        queue.push(item);
        writeActionQueue(queue);
    }

    function drainQueuedMutations() {
        var queue = readQueue();
        writeQueue([]);
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

    function readLocalRouteMetadata() {
        var meta = localMeta();
        if (!meta) {
            return null;
        }
        var syncTables = parseCsvAttribute(meta.getAttribute('data-ihp-local-sync-tables'));
        var reconnectProbePath = meta.getAttribute('data-ihp-local-reconnect-probe-path') || '';
        var conflictPolicy = meta.getAttribute('data-ihp-local-conflict-policy') || 'server-wins';
        var conflictField = meta.getAttribute('data-ihp-local-conflict-field') || '';
        var explicitRoutePath = meta.getAttribute('data-ihp-local-route');
        return {
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

    async function openPGlite(dataDir) {
        var db = null;
        if (window.PGlite && typeof window.PGlite.create === 'function') {
            try {
                db = await window.PGlite.create({ dataDir: dataDir });
            } catch (_error) {
                // Fall back to constructor-based initialization for compatibility.
            }
        }

        if (!db) {
            db = await new window.PGlite(dataDir);
        }

        // Probe the DB early so we can fall back to a different filesystem backend
        // before returning a runtime that will fail on first real query.
        if (db && typeof db.query === 'function') {
            await db.query('SELECT 1');
        } else if (db && typeof db.exec === 'function') {
            await db.exec('SELECT 1');
        }

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
        return url;
    }

    function sendDataSyncMessageOverWebSocket(payload) {
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
                    resolve(JSON.parse(event.data));
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
            return;
        }
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
            throw new Error('Replay action request failed with status ' + String(response.status));
        }
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
            return response.ok || (response.status >= 300 && response.status < 400);
        } catch (_error) {
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
                console.error('[ihp-local-runtime] conflict resolver hook failed', error);
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
                return;
            }
            this.syncState = state;
            document.dispatchEvent(new CustomEvent('ihp:sync:state', {
                detail: Object.assign({ state: state }, detail || {}),
            }));
        },

        getConnectivityProbeConfig: function () {
            var metadata = readLocalRouteMetadata();
            return {
                path: resolveConnectivityProbePath(metadata),
                timeoutMs: metadata ? metadata.reconnectProbeTimeoutMs : 2000,
                intervalMs: metadata ? metadata.reconnectProbeIntervalMs : 15000,
            };
        },

        ensureOnlineWithProbe: async function () {
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
                return true;
            }
            this.setSyncState('offline', { reason: 'probe-failed', path: config.path });
            return false;
        },

        restartConnectivityProbes: function () {
            if (this.connectivityProbeTimer) {
                clearInterval(this.connectivityProbeTimer);
                this.connectivityProbeTimer = null;
            }
            var self = this;
            var config = this.getConnectivityProbeConfig();
            if (!config.intervalMs || config.intervalMs <= 0) {
                return;
            }
            this.connectivityProbeTimer = setInterval(function () {
                self.ensureOnlineWithProbe().then(function (online) {
                    if (!online) {
                        return;
                    }
                    self.replayQueuedMutations().catch(function (error) {
                        self.setSyncState('error', { error: error && error.message ? error.message : String(error) });
                        console.error('[ihp-local-runtime] replay failed', error);
                    });
                }).catch(function (error) {
                    self.setSyncState('error', { error: error && error.message ? error.message : String(error) });
                    console.error('[ihp-local-runtime] connectivity probe failed', error);
                });
            }, config.intervalMs);
        },

        isReadyForLocal: function () {
            return isLocalRouteActive() && !this.isBrowserOnline();
        },

        registerRenderer: function (routePath, renderFn) {
            LOCAL_RENDERERS[routePath] = renderFn;
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
        },

        registerConflictResolver: function (table, resolver) {
            if (!table || typeof resolver !== 'function') {
                throw new Error('registerConflictResolver expects a table name and resolver function');
            }
            LOCAL_CONFLICT_RESOLVERS[String(table)] = resolver;
        },

        unregisterConflictResolver: function (table) {
            if (!table) {
                return;
            }
            delete LOCAL_CONFLICT_RESOLVERS[String(table)];
        },

        getCurrentMetadata: function () {
            return readLocalRouteMetadata();
        },

        unregisterAction: function (routePath) {
            if (!routePath) {
                return;
            }
            delete LOCAL_ACTIONS[normalizeRoutePath(routePath)];
        },

        registerRemoteDispatcher: function (remoteDispatcher) {
            this.remoteDispatcher = remoteDispatcher;
            if (this.isOnline()) {
                this.replayQueuedMutations().catch(function (error) {
                    console.error('[ihp-local-runtime] replay failed', error);
                });
            }
        },

        setCurrentUser: function (userId) {
            setCurrentUserId(userId);
        },

        clearCurrentUserData: async function () {
            var userId = getCurrentUserId();
            localStorage.removeItem(LOCAL_QUEUE_KEY_PREFIX + userId);
            localStorage.removeItem(LOCAL_FAILED_QUEUE_KEY_PREFIX + userId);
            localStorage.removeItem(LOCAL_ACTION_QUEUE_KEY_PREFIX + userId);
            localStorage.removeItem(LOCAL_USER_KEY);
            if (this.db && this.db.close) {
                await this.db.close();
            }
            this.db = null;
        },

        bootstrapSchema: async function (schemaSql) {
            this.bootstrappedSchemaSql = schemaSql;
            if (!this.db || !schemaSql || this.schemaBootstrapped) {
                return;
            }
            await this.db.exec(schemaSql);
            this.schemaBootstrapped = true;
        },

        ensureDb: async function () {
            if (this.db) {
                return this.db;
            }
            if (this.dbInitPromise) {
                return this.dbInitPromise;
            }

            var self = this;
            this.dbInitPromise = (async function () {
                if (!window.PGlite) {
                    throw new Error('PGlite is required for local-first runtime but window.PGlite is not available');
                }
                var filesystems = pickFilesystemCandidates();
                var lastError = null;
                for (var index = 0; index < filesystems.length; index += 1) {
                    try {
                        self.db = await openPGlite(filesystems[index]);
                        break;
                    } catch (error) {
                        lastError = error;
                    }
                }
                if (!self.db) {
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
                }
                if (self.bootstrappedSchemaSql && !self.schemaBootstrapped) {
                    await self.db.exec(self.bootstrappedSchemaSql);
                    self.schemaBootstrapped = true;
                }
                return self.db;
            })();

            try {
                return await this.dbInitPromise;
            } finally {
                this.dbInitPromise = null;
            }
        },

        refreshActiveLocalRoute: async function () {
            var metadata = readLocalRouteMetadata();
            if (!metadata || !metadata.routePath) {
                return;
            }
            var render = LOCAL_RENDERERS[metadata.routePath];
            if (!render) {
                document.dispatchEvent(new CustomEvent('ihp:local-refresh', { detail: { routePath: metadata.routePath } }));
                return;
            }
            var html = await render();
            if (typeof html === 'string' && html.length > 0) {
                document.dispatchEvent(new CustomEvent('ihp:local-refresh', { detail: { routePath: metadata.routePath, html: html } }));
            }
        },

        shouldHandleFormLocally: function (routePath, method) {
            if (!this.isReadyForLocal()) {
                return false;
            }
            var action = LOCAL_ACTIONS[normalizeRoutePath(routePath)];
            if (!action) {
                return false;
            }
            return action.methods.indexOf(String(method).toUpperCase()) !== -1;
        },

        executeRegisteredAction: async function (routePath, request) {
            var normalizedRoutePath = normalizeRoutePath(routePath);
            var action = LOCAL_ACTIONS[normalizedRoutePath];
            if (!action) {
                throw new Error('No local action registered for route: ' + normalizedRoutePath);
            }
            return await action.handler(request);
        },

        handleOfflineFormSubmission: async function (form, submitter) {
            var formData = createFormData(form, submitter);
            var method = readSubmissionMethod(form, formData);
            var routePath = readSubmissionPath(form);
            var replayPath = readSubmissionReplayPath(form);
            if (!this.shouldHandleFormLocally(routePath, method)) {
                return false;
            }
            var formFields = formDataToObject(formData);
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
                document.dispatchEvent(new CustomEvent('ihp:local-action:success', {
                    detail: { routePath: routePath, method: method, result: result },
                }));
                return true;
            } catch (error) {
                document.dispatchEvent(new CustomEvent('ihp:local-action:error', {
                    detail: { routePath: routePath, method: method, error: error },
                }));
                console.error('[ihp-local-runtime] local action failed', error);
                return true;
            }
        },

        installOfflineActionFormInterceptor: function () {
            if (localActionFormInterceptorInstalled) {
                return;
            }
            localActionFormInterceptorInstalled = true;
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
                event.preventDefault();
                self.handleOfflineFormSubmission(form, submitter).catch(function (error) {
                    console.error('[ihp-local-runtime] failed to handle local form submission', error);
                });
            }, true);
        },

        executeLocal: async function (payload) {
            var db = await this.ensureDb();

            switch (payload.tag) {
                case 'DataSyncQuery': {
                    var compiled = compileSelectQuery(payload.query);
                    var result = await db.query(compiled.sql, compiled.parameters);
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
            try {
                var response = await this.executeLocal(payload);
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
                    }
                    await this.refreshActiveLocalRoute();
                }
                return response;
            } catch (error) {
                return makeErrorResponse(payload, error);
            }
        },

        createRecord: async function (table, record, options) {
            record = ensureRecordId(record);
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
                return false;
            }
            var metadata = readLocalRouteMetadata();
            if (!metadata || !Array.isArray(metadata.syncTables) || metadata.syncTables.length === 0) {
                return true;
            }
            return metadata.syncTables.indexOf(table) !== -1;
        },

        syncDataSubscriptionSnapshot: async function (query, records) {
            if (!query || !query.table) {
                return;
            }
            if (!this.shouldMirrorTable(query.table)) {
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
                console.warn(
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
            } catch (error) {
                console.error('[ihp-local-runtime] failed to sync subscription snapshot', error);
            }
        },

        applyServerSubscriptionMessage: async function (query, message) {
            if (!query || !query.table || !message || !message.tag) {
                return;
            }
            if (!this.shouldMirrorTable(query.table)) {
                return;
            }
            var metadata = readLocalRouteMetadata();

            try {
                var db = await this.ensureDb();
                switch (message.tag) {
                    case 'DidInsert': {
                        if (!message.record) {
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
                        return;
                    }
                    case 'DidUpdate': {
                        if (!message.id) {
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
                        return;
                    }
                    case 'DidDelete': {
                        if (!message.id) {
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
                            return;
                        }
                        await db.query(
                            'DELETE FROM ' + quoteIdentifier(query.table) + ' WHERE id = ?',
                            [message.id]
                        );
                        return;
                    }
                    default:
                        return;
                }
            } catch (error) {
                console.error('[ihp-local-runtime] failed to apply server subscription message', error);
            }
        },

        replayQueuedActions: async function () {
            if (!this.isOnline()) {
                return;
            }
            var queuedActions = readActionQueue();
            if (!Array.isArray(queuedActions) || queuedActions.length === 0) {
                return;
            }

            var remaining = deepClone(queuedActions);
            for (var index = 0; index < queuedActions.length; index += 1) {
                var item = remaining[0];
                try {
                    await replayQueuedAction(item);
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
                    console.error('[ihp-local-runtime] replay action failed', replayError);
                    break;
                }
            }
            writeActionQueue(remaining);
        },

        replayQueuedMutations: async function () {
            if (this.replayInFlight) {
                return await this.replayInFlight;
            }

            var self = this;
            this.replayInFlight = (async function () {
                var isReachable = await self.ensureOnlineWithProbe();
                if (!isReachable) {
                    return;
                }
                self.setSyncState('syncing');
                await self.replayQueuedActions();
                var replayDispatcher = self.remoteDispatcher || sendDataSyncMessageOverWebSocket;
                var queued = readQueue();
                if (!Array.isArray(queued) || queued.length === 0) {
                    self.setSyncState('online');
                    return;
                }
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
                        if (replayPayload.tag === 'StartTransaction' && item.localTransactionId && response && response.transactionId) {
                            transactionIdMap[item.localTransactionId] = response.transactionId;
                            rewriteQueuedTransactionIds(remaining, item.localTransactionId, response.transactionId);
                        }
                        if (response && response.tag === 'DataSyncError') {
                            appendFailedMutation({
                                payload: item.payload,
                                response: response,
                                failedAt: new Date().toISOString(),
                            });
                            self.setSyncState('error', { error: response.errorMessage || 'replay mutation failed' });
                            console.error('[ihp-local-runtime] replay mutation failed', response);
                            break;
                        }
                        remaining.shift();
                    } catch (error) {
                        var replayErrorResponse = makeErrorResponse(item.payload, error);
                        appendFailedMutation({
                            payload: item.payload,
                            response: replayErrorResponse,
                            failedAt: new Date().toISOString(),
                        });
                        self.setSyncState('error', { error: replayErrorResponse.errorMessage || 'replay mutation failed' });
                        console.error('[ihp-local-runtime] replay mutation failed', replayErrorResponse);
                        break;
                    }
                }
                writeQueue(remaining);
                if (remaining.length === 0 && readActionQueue().length === 0) {
                    self.setSyncState('online');
                }
            })();

            try {
                return await this.replayInFlight;
            } finally {
                self.replayInFlight = null;
            }
        },

        getFailedMutations: function () {
            return readFailedQueue();
        },

        initialize: function () {
            if (this.initialized) {
                return;
            }
            this.initialized = true;
            var self = this;
            window.addEventListener('offline', function () {
                self.setSyncState('offline', { reason: 'browser-offline' });
            });
            window.addEventListener('online', function () {
                self.replayQueuedMutations().catch(function (error) {
                    self.setSyncState('error', { error: error && error.message ? error.message : String(error) });
                    console.error('[ihp-local-runtime] replay failed', error);
                });
            });
            window.addEventListener('turbolinks:load', function () {
                self.restartConnectivityProbes();
            });
            self.restartConnectivityProbes();
            if (self.isBrowserOnline()) {
                self.replayQueuedMutations().catch(function (error) {
                    self.setSyncState('error', { error: error && error.message ? error.message : String(error) });
                    console.error('[ihp-local-runtime] replay failed', error);
                });
            } else {
                self.setSyncState('offline', { reason: 'browser-offline' });
            }
        },
    };

    LocalRuntime.initialize();
    window.IHPLocalRuntime = LocalRuntime;
})();
