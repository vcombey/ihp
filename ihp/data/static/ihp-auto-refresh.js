var socket = null;
var sessionId = null;
var autoRefreshPaused = false;
var localRefreshListenerInstalled = false;
var LOCAL_DEBUG_FLAG_KEY = 'ihp_local_debug';
var AUTO_REFRESH_LOG_PREFIX = '[ihp-auto-refresh]';

function isAutoRefreshDebugEnabled() {
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

function autoRefreshLog(message, details) {
    if (!isAutoRefreshDebugEnabled()) {
        return;
    }
    if (details === undefined) {
        console.log(AUTO_REFRESH_LOG_PREFIX + ' ' + message);
        return;
    }
    console.log(AUTO_REFRESH_LOG_PREFIX + ' ' + message, details);
}

function autoRefreshWarn(message, details) {
    if (!isAutoRefreshDebugEnabled()) {
        return;
    }
    if (details === undefined) {
        console.warn(AUTO_REFRESH_LOG_PREFIX + ' ' + message);
        return;
    }
    console.warn(AUTO_REFRESH_LOG_PREFIX + ' ' + message, details);
}

function autoRefreshError(message, error, context) {
    if (!isAutoRefreshDebugEnabled()) {
        return;
    }
    var payload = { error: error };
    if (context && typeof context === 'object') {
        payload.context = context;
    }
    console.error(AUTO_REFRESH_LOG_PREFIX + ' ' + message, payload);
}

function isUuid(value) {
    return /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(String(value || ''));
}

function shouldUseLocalAutoRefresh(metaTag) {
    if (!metaTag) {
        autoRefreshLog('No meta tag available, local auto refresh disabled');
        return false;
    }

    var hasLocalRoute = metaTag.getAttribute('data-ihp-local-route') || metaTag.getAttribute('property') === 'ihp-local-route';
    if (!hasLocalRoute) {
        autoRefreshLog('Meta tag does not represent a local route');
        return false;
    }

    if (window.IHPLocalRuntime && window.IHPLocalRuntime.isReadyForLocal) {
        var readyForLocal = window.IHPLocalRuntime.isReadyForLocal();
        autoRefreshLog('Evaluated shouldUseLocalAutoRefresh via IHPLocalRuntime.isReadyForLocal()', {
            readyForLocal: readyForLocal,
            route: metaTag.getAttribute('content'),
            property: metaTag.getAttribute('property'),
        });
        return readyForLocal;
    }

    var fallbackOffline = navigator.onLine === false;
    autoRefreshLog('Evaluated shouldUseLocalAutoRefresh via navigator.onLine fallback', {
        fallbackOffline: fallbackOffline,
    });
    return fallbackOffline;
}

function installLocalRefreshListener() {
    if (localRefreshListenerInstalled) {
        autoRefreshLog('Local refresh listener already installed');
        return;
    }
    localRefreshListenerInstalled = true;
    autoRefreshLog('Installing local refresh listener for ihp:local-refresh events');
    document.addEventListener('ihp:local-refresh', function (event) {
        if (!event || !event.detail || !event.detail.html) {
            autoRefreshLog('Ignoring ihp:local-refresh event without HTML payload', event && event.detail ? event.detail : null);
            return;
        }
        autoRefreshLog('Applying local refresh HTML from ihp:local-refresh event', {
            routePath: event.detail.routePath || null,
            htmlLength: event.detail.html.length,
        });
        applyAutoRefreshHtml(event.detail.html);
    });
}

function applyAutoRefreshHtml(html) {
    autoRefreshLog('applyAutoRefreshHtml called', { htmlLength: html ? html.length : 0, paused: autoRefreshPaused });
    var parser = new DOMParser();
    var dom = parser.parseFromString(html, 'text/html');

    if (autoRefreshPaused) {
        autoRefreshWarn('Skipping auto-refresh HTML application because auto refresh is paused');
        return;
    }

    morphdom(document.body, dom.body, {
        getNodeKey: function (el) {
            var key = el.id;
            if (el.id) {
                key = el.id;
            } else if (el.form && el.name) {
                key = el.name + "_" + el.form.action;
            } else if (el instanceof HTMLFormElement) {
                key = "form#" + el.action;
            } else if (el instanceof HTMLScriptElement) {
                key = el.src;
            }
            return key;
        },
        onBeforeElChildrenUpdated: function(fromEl, toEl) {
            if (fromEl.tagName === 'TEXTAREA' || fromEl.tagName === 'INPUT') {
                toEl.checked = fromEl.checked;
                toEl.value = fromEl.value;
            } else if (fromEl.tagName === 'OPTION') {
                toEl.selected = fromEl.selected;
            }
        }
    });

    if (typeof window.clearAllIntervals === 'function') {
        window.clearAllIntervals();
    }
    if (typeof window.clearAllTimeouts === 'function') {
        window.clearAllTimeouts();
    }

    var turbolinksLoadEvent = new CustomEvent('turbolinks:load', {});
    document.dispatchEvent(turbolinksLoadEvent);
    autoRefreshLog('Dispatched synthetic turbolinks:load after DOM morph');
}

function autoRefreshView() {
    autoRefreshLog('autoRefreshView invoked');
    installLocalRefreshListener();

    var metaTag = document.querySelector('meta[property="ihp-auto-refresh-id"]');
    if (!metaTag) {
        metaTag = document.querySelector('meta[property="ihp-local-route"]');
    }

    if (!metaTag) {
        autoRefreshLog('No auto-refresh/local meta tag found; closing socket if needed');
        if (socket) {
            socket.close();
            socket = null;
            autoRefreshLog('Closed websocket because page has no auto-refresh meta');
        }
        return;
    }
    autoRefreshLog('Found auto-refresh meta tag', {
        property: metaTag.getAttribute('property'),
        content: metaTag.getAttribute('content'),
        hasLocalRouteAttr: !!metaTag.getAttribute('data-ihp-local-route'),
    });

    if (shouldUseLocalAutoRefresh(metaTag)) {
        autoRefreshLog('Using local auto-refresh mode for current route');
        if (socket) {
            socket.close();
            socket = null;
            autoRefreshLog('Closed websocket because local mode is active');
        }
        installLocalRefreshListener();
        if (window.IHPLocalRuntime && typeof window.IHPLocalRuntime.refreshActiveLocalRoute === "function") {
            autoRefreshLog('Requesting local runtime to refresh active local route');
            window.IHPLocalRuntime.refreshActiveLocalRoute().catch(function (error) {
                autoRefreshError('Local refresh failed', error);
            });
        }
        return;
    }

    if (metaTag.getAttribute('property') !== 'ihp-auto-refresh-id' || !isUuid(metaTag.content)) {
        autoRefreshWarn('Skipping websocket auto-refresh because meta tag is not ihp-auto-refresh-id UUID', {
            property: metaTag.getAttribute('property'),
            content: metaTag.getAttribute('content'),
        });
        if (socket) {
            socket.close();
            socket = null;
            autoRefreshLog('Closed websocket due to invalid auto-refresh session metadata');
        }
        return;
    }

    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    var socketHost = socketProtocol + "://" + window.location.hostname + ":" + document.location.port + '/AutoRefreshWSApp';
    autoRefreshLog('Preparing websocket auto-refresh connection', { socketHost: socketHost, sessionId: metaTag.content });
    if (socket && metaTag.content === sessionId) {
        autoRefreshLog('Auto-refresh websocket already connected for session', { sessionId: sessionId });
        return;
    } else if (socket) {
        socket.close();
        socket = new WebSocket(socketHost);
        sessionId = metaTag.content;
    } else {
        socket = new WebSocket(socketHost);
        sessionId = metaTag.content;
    }

    autoRefreshPaused = false;

    socket.onopen = function () {
        autoRefreshLog('Auto-refresh websocket opened, sending session id', { sessionId: metaTag.content });
        socket.send(metaTag.content);
    };

    socket.onmessage = function (event) {
        autoRefreshLog('Received auto-refresh websocket message', {
            sessionId: sessionId,
            htmlLength: event && event.data ? String(event.data).length : 0,
        });
        applyAutoRefreshHtml(event.data);
    };

    socket.onerror = function (error) {
        autoRefreshError('Auto-refresh websocket error', error, { sessionId: sessionId });
    };

    socket.onclose = function (event) {
        autoRefreshWarn('Auto-refresh websocket closed', {
            sessionId: sessionId,
            code: event && typeof event.code === 'number' ? event.code : null,
            reason: event && event.reason ? event.reason : '',
            wasClean: !!(event && event.wasClean),
        });
    };
}

/* Called by helpers.js when a form was just submitted and we're waiting for a response from the server */
window.pauseAutoRefresh = function () {
    autoRefreshPaused = true;
    autoRefreshLog('Auto-refresh paused by helpers.js');
};

if (window.Turbolinks) {
    autoRefreshLog('Turbolinks detected; binding autoRefreshView to turbolinks:load');
    document.addEventListener('turbolinks:load', autoRefreshView);
} else {
    autoRefreshLog('Turbolinks not detected; running autoRefreshView immediately');
    autoRefreshView();
}
