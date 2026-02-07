(function () {
    var LOCAL_USER_KEY = 'ihp_local_user_id';
    var LOCAL_QUEUE_KEY_PREFIX = 'ihp_local_queue:';
    var LOCAL_FAILED_QUEUE_KEY_PREFIX = 'ihp_local_failed_queue:';
    var LOCAL_DB_PREFIX = 'idb://ihp-local-db:';
    var LOCAL_RENDERERS = {};
    var LOCAL_ACTIONS = {};
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

    function readLocalRouteMetadata() {
        var meta = localMeta();
        if (!meta) {
            return null;
        }
        var explicitRoutePath = meta.getAttribute('data-ihp-local-route');
        return {
            routePath: explicitRoutePath || meta.getAttribute('content'),
            routeId: meta.getAttribute('data-ihp-local-route-id') || null,
            syncPolicy: meta.getAttribute('data-ihp-local-sync-policy') || 'server-wins',
            authPolicy: meta.getAttribute('data-ihp-local-auth-policy') || 'last-authenticated-user',
            schemaPolicy: meta.getAttribute('data-ihp-local-schema-policy') || 'whole-app',
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

    function pickFilesystem() {
        var scopedUser = sanitizeDbKey(getCurrentUserId());
        if (typeof navigator !== 'undefined' && 'storage' in navigator && navigator.storage && navigator.storage.getDirectory) {
            return 'opfs-ahp://ihp-local-' + scopedUser;
        }
        return userScopedDbPath();
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

    function normalizeActionMethods(methods) {
        var defaultMethods = ['POST', 'PUT', 'PATCH', 'DELETE'];
        var source = Array.isArray(methods) && methods.length > 0 ? methods : defaultMethods;
        return source.map(function (method) {
            return String(method).toUpperCase();
        });
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

    function makeErrorResponse(payload, error) {
        return {
            tag: 'DataSyncError',
            requestId: payload.requestId,
            errorMessage: error && error.message ? error.message : String(error),
        };
    }

    var LocalRuntime = {
        db: null,
        dbInitPromise: null,
        schemaBootstrapped: false,
        bootstrappedSchemaSql: null,
        activeTransactions: {},
        remoteDispatcher: null,
        initialized: false,

        isOnline: function () {
            return typeof navigator === 'undefined' ? true : navigator.onLine !== false;
        },

        isReadyForLocal: function () {
            return isLocalRouteActive() && !this.isOnline();
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

        unregisterAction: function (routePath) {
            if (!routePath) {
                return;
            }
            delete LOCAL_ACTIONS[normalizeRoutePath(routePath)];
        },

        registerRemoteDispatcher: function (remoteDispatcher) {
            this.remoteDispatcher = remoteDispatcher;
        },

        setCurrentUser: function (userId) {
            setCurrentUserId(userId);
        },

        clearCurrentUserData: async function () {
            var userId = getCurrentUserId();
            localStorage.removeItem(LOCAL_QUEUE_KEY_PREFIX + userId);
            localStorage.removeItem(LOCAL_FAILED_QUEUE_KEY_PREFIX + userId);
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
                var filesystem = pickFilesystem();
                try {
                    self.db = await window.PGlite.create({ dataDir: filesystem });
                } catch (_error) {
                    self.db = await new window.PGlite(filesystem);
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
            if (!this.shouldHandleFormLocally(routePath, method)) {
                return false;
            }
            var formFields = formDataToObject(formData);
            try {
                var result = await this.executeRegisteredAction(routePath, {
                    routePath: routePath,
                    method: method,
                    form: form,
                    submitter: submitter || null,
                    formData: formData,
                    formFields: formFields,
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
                    var queueItem = {
                        payload: deepClone(payload),
                        createdAt: new Date().toISOString(),
                    };
                    if (payload.tag === 'StartTransaction' && response && response.transactionId) {
                        queueItem.localTransactionId = response.transactionId;
                    }
                    appendQueuedMutation(queueItem);
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
            return await this.dispatchDataSync({
                tag: 'CreateRecordMessage',
                table: table,
                record: record,
                transactionId: transactionId,
            });
        },

        updateRecord: async function (table, id, patch, options) {
            var transactionId = options && options.transactionId ? options.transactionId : null;
            return await this.dispatchDataSync({
                tag: 'UpdateRecordMessage',
                table: table,
                id: id,
                patch: patch,
                transactionId: transactionId,
            });
        },

        deleteRecord: async function (table, id, options) {
            var transactionId = options && options.transactionId ? options.transactionId : null;
            return await this.dispatchDataSync({
                tag: 'DeleteRecordMessage',
                table: table,
                id: id,
                transactionId: transactionId,
            });
        },

        replayQueuedMutations: async function () {
            if (!this.remoteDispatcher || !this.isOnline()) {
                return;
            }
            var queued = drainQueuedMutations();
            var transactionIdMap = {};
            for (var i = 0; i < queued.length; i += 1) {
                var item = queued[i];
                try {
                    var replayPayload = deepClone(item.payload);
                    if (replayPayload.transactionId) {
                        var mappedTransactionId = transactionIdMap[replayPayload.transactionId];
                        if (!mappedTransactionId) {
                            throw new Error('Missing replay transaction mapping for transactionId=' + replayPayload.transactionId);
                        }
                        replayPayload.transactionId = mappedTransactionId;
                    }
                    if ((replayPayload.tag === 'CommitTransaction' || replayPayload.tag === 'RollbackTransaction') && replayPayload.id) {
                        var mappedId = transactionIdMap[replayPayload.id];
                        if (!mappedId) {
                            throw new Error('Missing replay transaction mapping for transaction id=' + replayPayload.id);
                        }
                        replayPayload.id = mappedId;
                    }

                    var response = await this.remoteDispatcher(replayPayload);
                    if (replayPayload.tag === 'StartTransaction' && item.localTransactionId && response && response.transactionId) {
                        transactionIdMap[item.localTransactionId] = response.transactionId;
                    }
                    if (response && response.tag === 'DataSyncError') {
                        appendFailedMutation({
                            payload: item.payload,
                            response: response,
                            failedAt: new Date().toISOString(),
                        });
                    }
                } catch (error) {
                    appendFailedMutation({
                        payload: item.payload,
                        response: makeErrorResponse(item.payload, error),
                        failedAt: new Date().toISOString(),
                    });
                }
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
            window.addEventListener('online', function () {
                self.replayQueuedMutations().catch(function (error) {
                    console.error('[ihp-local-runtime] replay failed', error);
                });
            });
        },
    };

    LocalRuntime.initialize();
    window.IHPLocalRuntime = LocalRuntime;
})();
