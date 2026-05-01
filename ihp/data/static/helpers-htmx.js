(function () {
    var ihpLoadEvent = new Event('ihp:load');
    var ihpUnloadEvent = new Event('ihp:unload');

    function toArray(nodeList) {
        return Array.prototype.slice.call(nodeList || []);
    }

    function selectAll(root, selector) {
        var scope = root || document;
        var result = [];

        if (scope.matches && scope.matches(selector)) {
            result.push(scope);
        }

        if (scope.querySelectorAll) {
            result = result.concat(toArray(scope.querySelectorAll(selector)));
        }

        return result;
    }

    function dispatchIhpLoad() {
        document.dispatchEvent(ihpLoadEvent);
    }

    function dispatchIhpUnload() {
        document.dispatchEvent(ihpUnloadEvent);
    }

    function clearTrackedTimers() {
        if (typeof window.clearAllIntervals === 'function') {
            window.clearAllIntervals();
        }

        if (typeof window.clearAllTimeouts === 'function') {
            window.clearAllTimeouts();
        }
    }

    function isBoostedPageSwap(event) {
        return !!(
            event &&
            event.detail &&
            event.detail.boosted &&
            event.detail.shouldSwap !== false
        );
    }

    function applyToggleInput(input) {
        var selector = input.getAttribute('data-toggle');
        if (!selector) {
            return;
        }

        toArray(document.querySelectorAll(selector)).forEach(function (el) {
            if (!(el instanceof HTMLElement)) {
                return;
            }

            if (input.checked) {
                el.removeAttribute('disabled');
            } else {
                el.setAttribute('disabled', 'disabled');
            }
        });
    }

    function handleToggleChange(event) {
        applyToggleInput(event.currentTarget);
    }

    function initToggle(root) {
        selectAll(root, '[data-toggle]').forEach(function (input) {
            if (!(input instanceof HTMLInputElement)) {
                return;
            }

            if (!input.__ihpToggleInitialized) {
                input.addEventListener('change', handleToggleChange);
                input.__ihpToggleInitialized = true;
            }

            applyToggleInput(input);
        });
    }

    function initTime(root) {
        if (window.timeago) {
            window.timeago().render(selectAll(root, '.time-ago'));
        }

        selectAll(root, '.date-time').forEach(function (elem) {
            var date = new Date(elem.dateTime);
            elem.innerHTML =
                date.toLocaleDateString() +
                ', ' +
                date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
        });

        selectAll(root, '.date').forEach(function (elem) {
            var date = new Date(elem.dateTime);
            elem.innerHTML = date.toLocaleDateString();
        });

        selectAll(root, '.time').forEach(function (elem) {
            var date = new Date(elem.dateTime);
            elem.innerHTML = date.toLocaleTimeString([], {
                hour: '2-digit',
                minute: '2-digit',
            });
        });
    }

    function handleFilePreviewChange(event) {
        var input = event.currentTarget;
        var previewSelector = input.getAttribute('data-preview');
        if (!previewSelector || !input.files || !input.files[0]) {
            return;
        }

        var target = document.querySelector(previewSelector);
        if (!target) {
            return;
        }

        var reader = new FileReader();
        reader.onload = function (e) {
            target.setAttribute('src', e.target.result);
        };
        reader.readAsDataURL(input.files[0]);
    }

    function initFileUploadPreview(root) {
        selectAll(root, 'input[type="file"]').forEach(function (input) {
            if (!(input instanceof HTMLInputElement)) {
                return;
            }

            if (!input.getAttribute('data-preview')) {
                return;
            }

            if (!input.__ihpFileUploadPreviewInitialized) {
                input.addEventListener('change', handleFilePreviewChange);
                input.__ihpFileUploadPreviewInitialized = true;
            }
        });
    }

    function initDatePicker(root) {
        if (!('flatpickr' in window)) {
            return;
        }

        selectAll(root, "input[type='date']").forEach(function (el) {
            if (el._flatpickr) {
                return;
            }

            var dateOptions = {};
            if (!el.dataset.altFormat) {
                dateOptions.altFormat = 'd.m.y';
            }
            if (!el.dataset.altInput) {
                dateOptions.altInput = true;
            }

            flatpickr(el, dateOptions);
        });

        selectAll(root, "input[type='datetime-local']").forEach(function (el) {
            if (el._flatpickr) {
                return;
            }

            var datetimeOptions = {};
            if (!el.dataset.enableTime) {
                datetimeOptions.enableTime = true;
            }
            if (!el.dataset.time_24hr) {
                datetimeOptions.time_24hr = true;
            }
            if (!el.dataset.dateFormat) {
                datetimeOptions.dateFormat = 'Z';
            }
            if (!el.dataset.altFormat) {
                datetimeOptions.altFormat = 'd.m.y, H:i';
            }
            if (!el.dataset.altInput) {
                datetimeOptions.altInput = true;
            }

            flatpickr(el, datetimeOptions);
        });
    }

    function initScrollIntoView(root) {
        var delay = window.unsafeSetTimeout || window.setTimeout;
        delay(function () {
            selectAll(root, '.js-scroll-into-view').forEach(function (el) {
                if (el && el.scrollIntoView) {
                    el.scrollIntoView({ behavior: 'smooth', block: 'start' });
                }
            });
        }, 1);
    }

    function handleBackClick(event) {
        event.preventDefault();
        var element = event.currentTarget;
        element.setAttribute('disabled', 'disabled');
        element.classList.add('disabled');
        window.history.back();
    }

    function initBack(root) {
        selectAll(root, '.js-back, [data-js-back]').forEach(function (element) {
            if (element.__ihpBackInitialized) {
                return;
            }

            if (element instanceof HTMLButtonElement || element.hasAttribute('data-js-back')) {
                element.addEventListener('click', handleBackClick);
            } else if (element instanceof HTMLAnchorElement && element.classList.contains('js-back')) {
                console.error(
                    'js-back does not supports <a> elements, use a <button> instead',
                    element
                );
            }

            element.__ihpBackInitialized = true;
        });
    }

    function resetLoadingLinks(root) {
        selectAll(root, '[data-loading-link]').forEach(function (element) {
            if (!(element instanceof HTMLElement)) {
                return;
            }

            element.classList.remove('is-loading');
            element.removeAttribute('aria-disabled');
        });
    }

    function dismissFormAlerts(form) {
        if (!form) {
            return;
        }

        toArray(form.querySelectorAll('.alert')).forEach(function (alert) {
            if (alert instanceof HTMLDivElement) {
                alert.classList.add('dismiss');
            }
        });
    }

    function flashAlertClass(kind) {
        return kind === 'success'
            ? 'alert alert-success alert-dismissible fade show'
            : 'alert alert-danger alert-dismissible fade show';
    }

    function renderFlashMessage(message) {
        if (!message || typeof message.message !== 'string') {
            return null;
        }

        var alert = document.createElement('div');
        alert.className = flashAlertClass(message.kind);
        alert.setAttribute('role', 'alert');

        var body = document.createElement('div');
        body.textContent = message.message;
        alert.appendChild(body);

        var dismissButton = document.createElement('button');
        dismissButton.type = 'button';
        dismissButton.className = 'btn-close';
        dismissButton.setAttribute('data-bs-dismiss', 'alert');
        dismissButton.setAttribute('aria-label', message.dismissLabel || 'Close');
        dismissButton.setAttribute('data-posthog-id', 'flash-dismiss-' + (message.kind || 'error'));
        alert.appendChild(dismissButton);

        return alert;
    }

    function replaceFlashMessages(messages) {
        var container = document.getElementById('flash-messages-container');
        if (!(container instanceof HTMLElement)) {
            return;
        }

        container.replaceChildren();

        if (!Array.isArray(messages) || messages.length === 0) {
            return;
        }

        var grid = document.createElement('div');
        grid.className = 'd-grid gap-2';

        messages.forEach(function (message) {
            var alert = renderFlashMessage(message);
            if (alert) {
                grid.appendChild(alert);
            }
        });

        if (grid.childElementCount > 0) {
            container.appendChild(grid);
        }
    }

    function flashMessagesFromEventDetail(detail) {
        if (!detail) {
            return [];
        }

        if (Array.isArray(detail.messages)) {
            return detail.messages;
        }

        if (detail.value && Array.isArray(detail.value.messages)) {
            return detail.value.messages;
        }

        return [];
    }

    function flashMessagesFromResponseHeader(xhr) {
        if (!xhr || typeof xhr.getResponseHeader !== 'function') {
            return [];
        }

        var raw = xhr.getResponseHeader('X-Gitoku-Flash');
        if (!raw) {
            return [];
        }

        try {
            var payload = JSON.parse(raw);
            return Array.isArray(payload.messages) ? payload.messages : [];
        } catch (_error) {
            return [];
        }
    }

    function initCompatibility(root, options) {
        var settings = options || {};

        initBack(root);
        initToggle(root);
        initTime(root);
        initDatePicker(root);
        initFileUploadPreview(root);

        if (settings.includeScrollIntoView) {
            initScrollIntoView(root);
        }
    }

    if (!('allIntervals' in window)) {
        window.allIntervals = [];
        window.allTimeouts = [];

        window.unsafeSetInterval = window.setInterval;
        window.unsafeSetTimeout = window.setTimeout;

        window.setInterval = function () {
            var id = window.unsafeSetInterval.apply(window, arguments);
            window.allIntervals.push(id);
            return id;
        };

        window.setTimeout = function () {
            var id = window.unsafeSetTimeout.apply(window, arguments);
            window.allTimeouts.push(id);
            return id;
        };

        window.clearAllIntervals = function () {
            for (var i = 0; i < window.allIntervals.length; i++) {
                clearInterval(window.allIntervals[i]);
            }

            var oldLength = window.allIntervals.length;
            window.allIntervals = new Array(oldLength);
        };

        window.clearAllTimeouts = function () {
            for (var i = 0; i < window.allTimeouts.length; i++) {
                clearTimeout(window.allTimeouts[i]);
            }

            var oldLength = window.allTimeouts.length;
            window.allTimeouts = new Array(oldLength);
        };
    }

    window.addEventListener('beforeunload', function () {
        dispatchIhpUnload();
    });

    document.addEventListener('click', function (event) {
        var target = event.target;
        var element =
            target && target.closest ? target.closest('[data-loading-link]') : null;
        var href;

        if (!(element instanceof HTMLAnchorElement)) {
            return;
        }

        if (event.defaultPrevented || event.button !== 0) {
            return;
        }

        if (event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) {
            return;
        }

        href = element.getAttribute('href');
        if (!href || href === '#') {
            return;
        }

        if (element.classList.contains('is-loading')) {
            event.preventDefault();
            return;
        }

        element.classList.add('is-loading');
        element.setAttribute('aria-disabled', 'true');
    });

    window.addEventListener('pageshow', function () {
        resetLoadingLinks(document);
    });

    document.addEventListener('DOMContentLoaded', function () {
        initCompatibility(document, { includeScrollIntoView: true });
        dispatchIhpLoad();
    });

    if (window.htmx) {
        document.addEventListener('htmx:beforeSwap', function (event) {
            dispatchIhpUnload();

            if (isBoostedPageSwap(event)) {
                clearTrackedTimers();
            }
        });

        document.addEventListener('htmx:load', function (event) {
            var root =
                event && event.detail && event.detail.elt
                    ? event.detail.elt
                    : event && event.target
                        ? event.target
                        : document;

            initCompatibility(root, { includeScrollIntoView: true });
            dispatchIhpLoad();
        });

        document.addEventListener('htmx:beforeRequest', function (event) {
            var trigger = event && event.detail ? event.detail.elt : null;
            var form = trigger && trigger.closest ? trigger.closest('form') : null;
            dismissFormAlerts(form);
        });

        document.addEventListener('htmx:afterRequest', function (event) {
            var xhr = event && event.detail ? event.detail.xhr : null;
            var messages = flashMessagesFromResponseHeader(xhr);
            if (messages.length > 0) {
                replaceFlashMessages(messages);
            }
        });

        document.addEventListener('gitoku:flash', function (event) {
            var detail = event && event.detail ? event.detail : null;
            replaceFlashMessages(flashMessagesFromEventDetail(detail));
        });
    }

    // HTMX morphdom swap extension.
    // Rationale:
    // - HTMX by default swaps via innerHTML; that nukes element identity and
    //   loses input state on full-page updates.
    // - morphdom preserves element identity by diffing the existing DOM, which
    //   keeps input values, cursor positions, and attached listeners stable.
    // - HTMX does not re-process nodes when the swap is handled by an extension,
    //   so we explicitly call htmx.process to activate new hx-* attributes.

    // Preserve user-entered values during swaps (especially for full-body refreshes).
    function syncInputValue(fromEl, toEl) {
        var tag = fromEl.tagName;
        if (tag === 'INPUT') {
            var type = (fromEl.getAttribute('type') || '').toLowerCase();
            if (type === 'checkbox' || type === 'radio') {
                toEl.checked = fromEl.checked;
            }
            if (type !== 'file') {
                toEl.value = fromEl.value;
            }
        } else if (tag === 'TEXTAREA') {
            toEl.value = fromEl.value;
        }
    }

    // Provide a stable key so morphdom can match nodes across updates.
    function getNodeKey(el) {
        if (el.id) {
            return el.id;
        }
        if (el instanceof HTMLScriptElement && el.src) {
            return el.src;
        }
        return undefined;
    }

    function resolveLiveTarget(target) {
        if (!target) {
            return target;
        }
        if (target === document.body) {
            return document.body;
        }
        if (target.id) {
            return document.getElementById(target.id) || target;
        }
        if (document.body.contains(target)) {
            return target;
        }
        return target;
    }

    function findSingleRootElement(fragment) {
        if (!fragment || fragment.nodeType !== 11) {
            return null;
        }

        var root = null;
        for (var i = 0; i < fragment.childNodes.length; i++) {
            var node = fragment.childNodes[i];
            if (node.nodeType === Node.COMMENT_NODE) {
                continue;
            }
            if (node.nodeType === Node.TEXT_NODE && node.textContent.trim() === '') {
                continue;
            }
            if (node.nodeType !== Node.ELEMENT_NODE) {
                return null;
            }
            if (root) {
                return null;
            }
            root = node;
        }

        return root;
    }

    function wrapToMatchTarget(target, content) {
        var wrapper = document.createElement(target.tagName);
        for (var i = 0; i < target.attributes.length; i++) {
            var attr = target.attributes[i];
            wrapper.setAttribute(attr.name, attr.value);
        }
        wrapper.appendChild(content);
        return wrapper;
    }

    function shouldReuseRoot(target, root) {
        return root.tagName === target.tagName && (!target.id || root.id === target.id);
    }

    // HTMX gives us a fragment; wrap it to match the target element for morphdom.
    // If the fragment already contains a single matching root element, reuse it
    // directly instead of nesting duplicate roots.
    function toSwapNode(target, fragment) {
        if (fragment && fragment.nodeType === 11) {
            var root = findSingleRootElement(fragment);
            if (root && shouldReuseRoot(target, root)) {
                return root;
            }

            return wrapToMatchTarget(target, fragment);
        }

        if (fragment && fragment.nodeType === Node.ELEMENT_NODE && !shouldReuseRoot(target, fragment)) {
            return wrapToMatchTarget(target, fragment);
        }
        return fragment;
    }

    // morphdom preserves element identity, which keeps inputs stable across swaps.
    // We also keep file inputs untouched because browsers restrict programmatic
    // value changes for security reasons.
    function morphdomSwap(target, fragment) {
        var swapNode = toSwapNode(target, fragment);
        morphdom(target, swapNode, {
            childrenOnly: false,
            getNodeKey: getNodeKey,
            onBeforeElUpdated: function (fromEl, toEl) {
                // File inputs are not safely transferable across DOM patches.
                if (fromEl.tagName === 'INPUT') {
                    var type = (fromEl.getAttribute('type') || '').toLowerCase();
                    if (type === 'file') {
                        return false;
                    }
                }
                syncInputValue(fromEl, toEl);
                return true;
            },
        });
    }

    if (window.htmx && window.morphdom) {
        htmx.defineExtension('morphdom-swap', {
            isInlineSwap: function (swapStyle) {
                return swapStyle === 'morphdom';
            },
            handleSwap: function (swapStyle, target, fragment) {
                if (swapStyle !== 'morphdom') return;
                // Swap using morphdom instead of the default innerHTML strategy.
                morphdomSwap(target, fragment);
                // Re-process the live DOM node, not the pre-morphdom target
                // reference, otherwise later HTMX interactions can stop working.
                window.setTimeout(function () {
                    htmx.process(resolveLiveTarget(target));
                }, 0);
                return true;
            },
        });
    }
})();
