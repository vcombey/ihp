(function () {
  if (window.__ihpAutoRefreshInitialized) {
    return;
  }
  window.__ihpAutoRefreshInitialized = true;

  var autoRefreshSessions = {};
  var autoRefreshPaused = false;
  var inflightRequests = 0;
  var autoRefreshReloadTimer = null;
  var autoRefreshReconnectStateKey = 'ihp:auto-refresh:reconnect';
  var autoRefreshReloadDelayMs = 1000;
  var autoRefreshReloadWindowMs = 15000;
  var autoRefreshReloadMaxAttempts = 3;
  var autoRefreshOnlineReloadRegistered = false;

  function shouldPreserveFieldValue(fromEl, toEl) {
    return !(
      (fromEl && fromEl.hasAttribute && fromEl.hasAttribute('data-auto-refresh-allow-server-value')) ||
      (toEl && toEl.hasAttribute && toEl.hasAttribute('data-auto-refresh-allow-server-value'))
    );
  }

  function syncFieldValue(fromEl, toEl) {
    var tag = fromEl.tagName;
    var shouldPreserve = shouldPreserveFieldValue(fromEl, toEl);

    if (tag === 'INPUT') {
      var type = (fromEl.getAttribute('type') || '').toLowerCase();

      if (type === 'checkbox' || type === 'radio') {
        if (shouldPreserve) {
          toEl.checked = fromEl.checked;
        }
      } else if (type !== 'file' && shouldPreserve) {
        toEl.value = fromEl.value;
      }
    } else if (tag === 'TEXTAREA') {
      if (shouldPreserve) {
        toEl.value = fromEl.value;
      }
    } else if (tag === 'SELECT') {
      if (shouldPreserve) {
        toEl.value = fromEl.value;
      }
    } else if (tag === 'OPTION') {
      toEl.selected = fromEl.selected;
    }
  }

  var morphdomOptions = {
    getNodeKey: function (el) {
      if (el.id) {
        return el.id;
      }
      if (el instanceof HTMLScriptElement) {
        return el.src;
      }
      return undefined;
    },
    onBeforeElUpdated: function (fromEl, toEl) {
      if (fromEl.tagName === 'INPUT') {
        var type = (fromEl.getAttribute('type') || '').toLowerCase();
        if (type === 'file') {
          return false;
        }
      }

      syncFieldValue(fromEl, toEl);
      return true;
    },
  };

  function childrenOnlyMorphdomOptions() {
    return {
      getNodeKey: morphdomOptions.getNodeKey,
      onBeforeElUpdated: morphdomOptions.onBeforeElUpdated,
      childrenOnly: true,
    };
  }

  function getMetaTarget(meta) {
    if (!meta) {
      return null;
    }
    var target = meta.getAttribute('data-ihp-auto-refresh-target');
    return target && target.length > 0 ? target : null;
  }

  function getRequestHeader(event, headerName) {
    return (
      event &&
      event.detail &&
      event.detail.requestConfig &&
      event.detail.requestConfig.headers &&
      event.detail.requestConfig.headers[headerName]
    );
  }

  function isPreloadedRequest(event) {
    return getRequestHeader(event, 'HX-Preloaded') === 'true';
  }

  function getSessionKey(config) {
    return config.target ? 'target:' + config.target : 'body';
  }

  function isTargetWithinSwap(targetSelector, swapTarget) {
    if (!swapTarget) {
      return true;
    }

    if (targetSelector === null || targetSelector === undefined) {
      return true;
    }

    var target = document.querySelector(targetSelector);
    if (!target) {
      return true;
    }

    return target === swapTarget || swapTarget.contains(target);
  }

  function clearAutoRefreshMeta() {
    var metas = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(metas, function (meta) {
      if (meta.parentNode) {
        meta.parentNode.removeChild(meta);
      }
    });
  }

  function clearAutoRefreshMetaForTarget(swapTarget) {
    if (!swapTarget) {
      clearAutoRefreshMeta();
      return;
    }

    var metas = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(metas, function (meta) {
      if (!isTargetWithinSwap(getMetaTarget(meta), swapTarget)) {
        return;
      }

      if (meta.parentNode) {
        meta.parentNode.removeChild(meta);
      }
    });
  }

  function isBoostedPageSwap(event) {
    return !!(
      event &&
      event.detail &&
      event.detail.boosted &&
      event.detail.shouldSwap !== false
    );
  }

  function hasAutoRefreshMeta() {
    return !!document.head.querySelector('meta[property="ihp-auto-refresh-id"]');
  }

  function readAutoRefreshReconnectState() {
    try {
      var rawState = window.sessionStorage.getItem(autoRefreshReconnectStateKey);
      if (!rawState) {
        return { count: 0, startedAt: 0 };
      }

      var parsedState = JSON.parse(rawState);
      if (
        !parsedState ||
        typeof parsedState.count !== 'number' ||
        typeof parsedState.startedAt !== 'number'
      ) {
        return { count: 0, startedAt: 0 };
      }

      if (Date.now() - parsedState.startedAt > autoRefreshReloadWindowMs) {
        return { count: 0, startedAt: 0 };
      }

      return parsedState;
    } catch (_error) {
      return { count: 0, startedAt: 0 };
    }
  }

  function writeAutoRefreshReconnectState(state) {
    try {
      if (!state || state.count <= 0) {
        window.sessionStorage.removeItem(autoRefreshReconnectStateKey);
        return;
      }

      window.sessionStorage.setItem(
        autoRefreshReconnectStateKey,
        JSON.stringify(state),
      );
    } catch (_error) {}
  }

  function resetAutoRefreshReconnectState() {
    writeAutoRefreshReconnectState({ count: 0, startedAt: 0 });
  }

  function registerAutoRefreshReconnectAttempt() {
    var currentState = readAutoRefreshReconnectState();
    var now = Date.now();
    var nextState;

    if (currentState.count <= 0 || now - currentState.startedAt > autoRefreshReloadWindowMs) {
      nextState = { count: 1, startedAt: now };
    } else {
      nextState = {
        count: currentState.count + 1,
        startedAt: currentState.startedAt,
      };
    }

    writeAutoRefreshReconnectState(nextState);
    return nextState.count <= autoRefreshReloadMaxAttempts;
  }

  function unregisterAutoRefreshOnlineReload() {
    if (!autoRefreshOnlineReloadRegistered) {
      return;
    }

    window.removeEventListener('online', scheduleAutoRefreshReload);
    autoRefreshOnlineReloadRegistered = false;
  }

  function cancelAutoRefreshReload() {
    if (autoRefreshReloadTimer) {
      window.clearTimeout(autoRefreshReloadTimer);
      autoRefreshReloadTimer = null;
    }
    unregisterAutoRefreshOnlineReload();
  }

  function scheduleAutoRefreshReload() {
    if (autoRefreshReloadTimer || !hasAutoRefreshMeta()) {
      return;
    }

    if (navigator.onLine === false) {
      if (!autoRefreshOnlineReloadRegistered) {
        window.addEventListener('online', scheduleAutoRefreshReload);
        autoRefreshOnlineReloadRegistered = true;
      }
      return;
    }

    unregisterAutoRefreshOnlineReload();

    if (!registerAutoRefreshReconnectAttempt()) {
      return;
    }

    autoRefreshReloadTimer = window.setTimeout(function () {
      autoRefreshReloadTimer = null;
      if (hasAutoRefreshMeta()) {
        window.location.reload();
      }
    }, autoRefreshReloadDelayMs);
  }

  function replaceAutoRefreshMeta(meta, fallbackTarget) {
    if (!meta) {
      return;
    }

    if (
      !meta.getAttribute('data-ihp-auto-refresh-target') &&
      fallbackTarget &&
      fallbackTarget.id
    ) {
      meta.setAttribute('data-ihp-auto-refresh-target', '#' + fallbackTarget.id);
    }

    var metaTarget = getMetaTarget(meta);
    var existing = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(existing, function (node) {
      if (getMetaTarget(node) === metaTarget && node.parentNode) {
        node.parentNode.removeChild(node);
      }
    });

    document.head.appendChild(meta);
  }

  function harvestAutoRefreshMeta(root, fallbackTarget) {
    if (!root || !root.querySelectorAll) {
      return;
    }

    var metas = root.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(metas, function (meta) {
      if (meta.parentNode === document.head) {
        return;
      }

      replaceAutoRefreshMeta(meta.cloneNode(true), fallbackTarget);

      if (meta.parentNode) {
        meta.parentNode.removeChild(meta);
      }
    });
  }

  function expectedMetaTarget(fallbackTarget) {
    if (fallbackTarget === null) {
      return null;
    }
    if (fallbackTarget && fallbackTarget.id) {
      return '#' + fallbackTarget.id;
    }
    return undefined;
  }

  function removeAutoRefreshMeta(metaTarget) {
    var existing = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(existing, function (node) {
      if (getMetaTarget(node) === metaTarget && node.parentNode) {
        node.parentNode.removeChild(node);
      }
    });
  }

  function syncAutoRefreshMeta(root, fallbackTarget) {
    var metaTarget = expectedMetaTarget(fallbackTarget);
    if (metaTarget !== undefined) {
      var metas = root ? root.querySelectorAll('meta[property="ihp-auto-refresh-id"]') : [];
      var hasMatchingMeta = false;

      Array.prototype.forEach.call(metas, function (meta) {
        var incomingTarget = getMetaTarget(meta);
        if (incomingTarget === null && metaTarget !== null) {
          incomingTarget = metaTarget;
        }
        if (incomingTarget === metaTarget) {
          hasMatchingMeta = true;
        }
      });

      if (!hasMatchingMeta) {
        removeAutoRefreshMeta(metaTarget);
      }
    }

    harvestAutoRefreshMeta(root, fallbackTarget);
  }

  function readAutoRefreshConfigs() {
    var metas = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    if (!metas || metas.length === 0) {
      return [];
    }

    var configs = [];
    var seen = {};

    Array.prototype.forEach.call(metas, function (meta) {
      if (!meta.content) {
        return;
      }

      var config = {
        sessionId: meta.content,
        target: getMetaTarget(meta),
      };
      var key = getSessionKey(config);
      if (seen[key]) {
        return;
      }

      seen[key] = true;
      configs.push(config);
    });

    return configs;
  }

  function serializedAutoRefreshSessionIds() {
    return readAutoRefreshConfigs()
      .map(function (config) {
        return config.sessionId;
      })
      .join('');
  }

  function socketHost(serializedSessionIds) {
    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    var url = (
      socketProtocol +
      '://' +
      window.location.hostname +
      ':' +
      document.location.port +
      '/AutoRefreshWSApp'
    );
    if (serializedSessionIds) {
      url += '?autoRefreshSessions=' + encodeURIComponent(serializedSessionIds);
    }
    return url;
  }

  function closeSession(key) {
    var session = autoRefreshSessions[key];
    if (!session) {
      return;
    }

    if (session.socket) {
      session.expectedClose = true;
      session.socket.close();
    }
    delete autoRefreshSessions[key];
  }

  function closeAllSessions() {
    Object.keys(autoRefreshSessions).forEach(function (key) {
      closeSession(key);
    });
  }

  function closeSessionsForTarget(swapTarget) {
    if (!swapTarget) {
      closeAllSessions();
      return;
    }

    Object.keys(autoRefreshSessions).forEach(function (key) {
      var session = autoRefreshSessions[key];
      if (!session) {
        return;
      }

      if (!isTargetWithinSwap(session.targetSelector, swapTarget)) {
        return;
      }

      closeSession(key);
    });
  }

  function applyHtmlUpdate(dom, session) {
    if (session.targetSelector) {
      var target = document.querySelector(session.targetSelector);
      if (!target) {
        return;
      }

      var newTarget = dom.querySelector(session.targetSelector);
      if (newTarget) {
        morphdom(target, newTarget, morphdomOptions);
      } else {
        morphdom(target, dom.body, childrenOnlyMorphdomOptions());
      }

      if (window.htmx) {
        htmx.process(target);
      }
      return;
    }

    morphdom(document.body, dom.body, morphdomOptions);
    if (window.htmx) {
      htmx.process(document.body);
    }
  }

  function afterMorphdomUpdate() {
    if (typeof window.clearAllIntervals === 'function') {
      window.clearAllIntervals();
    }
    if (typeof window.clearAllTimeouts === 'function') {
      window.clearAllTimeouts();
    }

    document.dispatchEvent(new CustomEvent('turbolinks:load', {}));
  }

  function handleIncomingHtml(html, session) {
    if (autoRefreshPaused) {
      session.pendingHtml = html;
      return;
    }

    session.pendingHtml = null;

    var dom = new DOMParser().parseFromString(html, 'text/html');
    var fallbackTarget = session.targetSelector
      ? document.querySelector(session.targetSelector)
      : null;

    syncAutoRefreshMeta(dom, fallbackTarget);
    autoRefreshView();

    applyHtmlUpdate(dom, session);
    afterMorphdomUpdate();
  }

  function harvestAutoRefreshMetaFromHtml(html, fallbackTarget) {
    if (!html) {
      return;
    }

    var dom = new DOMParser().parseFromString(html, 'text/html');
    syncAutoRefreshMeta(dom, fallbackTarget);
  }

  function openAutoRefreshSession(config, key) {
    var session = {
      sessionId: config.sessionId,
      targetSelector: config.target || null,
      socket: null,
      pendingHtml: null,
      expectedClose: false,
    };

    session.socket = new WebSocket(
      socketHost(serializedAutoRefreshSessionIds()),
    );

    session.socket.onopen = function () {
      cancelAutoRefreshReload();
      resetAutoRefreshReconnectState();
      session.socket.send(session.sessionId);
      document.dispatchEvent(
        new CustomEvent('ihp:auto-refresh-session-open', {
          detail: {
            targetSelector: session.targetSelector,
            sessionId: session.sessionId,
          },
        }),
      );
    };

    session.socket.onmessage = function (event) {
      handleIncomingHtml(event.data, session);
    };

    session.socket.onerror = function () {
      scheduleAutoRefreshReload();
    };

    session.socket.onclose = function () {
      if (autoRefreshSessions[key] === session) {
        delete autoRefreshSessions[key];
      }

      if (session.expectedClose) {
        return;
      }

      window.setTimeout(autoRefreshView, 100);
      scheduleAutoRefreshReload();
    };

    return session;
  }

  function autoRefreshView() {
    var configs = readAutoRefreshConfigs();

    if (!configs || configs.length === 0) {
      closeAllSessions();
      cancelAutoRefreshReload();
      resetAutoRefreshReconnectState();
      return;
    }

    var nextKeys = {};

    configs.forEach(function (config) {
      var key = getSessionKey(config);
      nextKeys[key] = true;

      var existing = autoRefreshSessions[key];
      if (existing && existing.sessionId === config.sessionId) {
        return;
      }

      if (existing) {
        closeSession(key);
      }

      autoRefreshSessions[key] = openAutoRefreshSession(config, key);
    });

    Object.keys(autoRefreshSessions).forEach(function (key) {
      if (!nextKeys[key]) {
        closeSession(key);
      }
    });
  }

  function resumePendingRefreshes() {
    Object.keys(autoRefreshSessions).forEach(function (key) {
      var session = autoRefreshSessions[key];
      if (!session || !session.pendingHtml) {
        return;
      }

      var html = session.pendingHtml;
      session.pendingHtml = null;
      handleIncomingHtml(html, session);
    });
  }

  window.pauseAutoRefresh = function () {
    autoRefreshPaused = true;
  };

  if (window.Turbolinks) {
    document.addEventListener('turbolinks:load', autoRefreshView);
  } else {
    autoRefreshView();
  }

  if (window.htmx) {
    document.addEventListener('htmx:beforeSwap', function (event) {
      var target =
        event && event.detail && event.detail.target
          ? event.detail.target
          : event.target;
      var responseText =
        event &&
        event.detail &&
        event.detail.xhr &&
        typeof event.detail.xhr.responseText === 'string'
          ? event.detail.xhr.responseText
          : null;

      if (isBoostedPageSwap(event)) {
        clearAutoRefreshMetaForTarget(target);
        closeSessionsForTarget(target);
      }

      harvestAutoRefreshMetaFromHtml(responseText, target);
    });

    document.addEventListener('htmx:beforeRequest', function (event) {
      if (isPreloadedRequest(event)) {
        return;
      }

      inflightRequests += 1;
      autoRefreshPaused = true;
    });

    document.addEventListener('htmx:configRequest', function (event) {
      if (!event || !event.detail || !event.detail.headers) {
        return;
      }

      var sessionIds = serializedAutoRefreshSessionIds();
      if (!sessionIds) {
        return;
      }

      event.detail.headers['X-IHP-Auto-Refresh-Sessions'] = sessionIds;
    });

    document.addEventListener('htmx:afterRequest', function (event) {
      if (isPreloadedRequest(event)) {
        return;
      }

      inflightRequests = Math.max(0, inflightRequests - 1);
      autoRefreshPaused = inflightRequests > 0;
      if (!autoRefreshPaused) {
        resumePendingRefreshes();
      }
    });

    document.addEventListener('htmx:afterSwap', function (event) {
      var target =
        event && event.detail && event.detail.target
          ? event.detail.target
          : event.target;

      harvestAutoRefreshMeta(target, target);
      window.setTimeout(autoRefreshView, 100);
    });
  }
})();
