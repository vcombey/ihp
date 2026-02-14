var socket = null;
var sessionId = null;
var autoRefreshPaused = false;
var localRefreshListenerInstalled = false;

function shouldUseLocalAutoRefresh(metaTag) {
    if (!metaTag) {
        return false;
    }

    var hasLocalRoute = metaTag.getAttribute('data-ihp-local-route') || metaTag.getAttribute('property') === 'ihp-local-route';
    if (!hasLocalRoute) {
        return false;
    }

    if (window.IHPLocalRuntime && window.IHPLocalRuntime.isReadyForLocal) {
        return window.IHPLocalRuntime.isReadyForLocal();
    }

    return navigator.onLine === false;
}

function installLocalRefreshListener() {
    if (localRefreshListenerInstalled) {
        return;
    }
    localRefreshListenerInstalled = true;
    document.addEventListener('ihp:local-refresh', function (event) {
        if (!event || !event.detail || !event.detail.html) {
            return;
        }
        applyAutoRefreshHtml(event.detail.html);
    });
}

function applyAutoRefreshHtml(html) {
    var parser = new DOMParser();
    var dom = parser.parseFromString(html, 'text/html');

    if (autoRefreshPaused) {
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
}

function autoRefreshView() {
    var metaTag = document.querySelector('meta[property="ihp-auto-refresh-id"]');
    if (!metaTag) {
        metaTag = document.querySelector('meta[property="ihp-local-route"]');
    }

    if (!metaTag) {
        if (socket) {
            socket.close();
            socket = null;
        }
        return;
    }

    if (shouldUseLocalAutoRefresh(metaTag)) {
        if (socket) {
            socket.close();
            socket = null;
        }
        installLocalRefreshListener();
        if (window.IHPLocalRuntime && typeof window.IHPLocalRuntime.refreshActiveLocalRoute === "function") {
            window.IHPLocalRuntime.refreshActiveLocalRoute().catch(function (error) {
                console.error("[ihp-auto-refresh] local refresh failed", error);
            });
        }
        return;
    }

    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    var socketHost = socketProtocol + "://" + window.location.hostname + ":" + document.location.port + '/AutoRefreshWSApp';
    if (socket && metaTag.content === sessionId) {
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
        socket.send(metaTag.content);
    };

    socket.onmessage = function (event) {
        applyAutoRefreshHtml(event.data);
    };
}

/* Called by helpers.js when a form was just submitted and we're waiting for a response from the server */
window.pauseAutoRefresh = function () {
    autoRefreshPaused = true;
};

if (window.Turbolinks) {
    document.addEventListener('turbolinks:load', autoRefreshView);
} else {
    autoRefreshView();
}

