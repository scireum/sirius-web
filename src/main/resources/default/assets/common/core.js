// Checks if the given value is empty or null.
sirius.isEmpty = function (value) {
    if (Array.isArray(value) && value.length === 0) {
        return true;
    }

    return typeof value === 'undefined' || value === null || value === undefined || value === '';
}

// Checks if the given value is non-empty.
sirius.isFilled = function (value) {
    return !sirius.isEmpty(value);
}

// Executes the given callback once the DOM is completely loaded.
// This is equivalent to $(document).ready().
sirius.ready = function (callback) {
    // Add as listener in case DOM is loading...
    document.addEventListener("DOMContentLoaded", callback);
    // Call manually is we're late...
    if (document.readyState === "interactive" || document.readyState === "complete" ) {
        callback();
    }
}

// Tries to find the parent node of the given type.
// Note that the type is UPPERCASE like 'FORM'.
sirius.findParentOfType = function(_node, type) {
    let _parent = _node.parentNode;
    while(_parent != null) {
        if (_parent.nodeName === type) {
            return _parent;
        } else {
            _parent = _parent.parentNode;
        }
    }

    return null;
}

// Invokes the given listener if enter is pressed in the given input field.
sirius.addEnterListener = function(_node, listener) {
    _node.addEventListener('keydown', function(event) {
        if (event.code === 'Enter') {
            event.preventDefault();
            listener(event);
        }
    });
}

sirius.getJSON = function(url, params) {
    if (!url.endsWith('?')) {
        url = url + '?';
    }
    Object.keys(params).forEach(function(key) {
        if (!url.endsWith('?')) {
            url += '&';
        }
        url += key + '=' + encodeURIComponent(params[key]);
    });

    return fetch(url, {
        method: "get"
    }).then(function (response) {
        return response.json();
    });
}

sirius.postJSON = function(url, params) {
    let formData = new FormData();
    Object.keys(params).forEach(function(key) {
        formData.append(key, params[key]);
    });

    return fetch(url, {
        method: "post",
        body: formData
    }).then(function (response) {
        return response.json();
    });
}

/**
 * Throttles a javascript function by calling it only every x milliseconds.
 * <p>
 * The throttled function is called once immediately, and then every x milliseconds.
 *
 * @param fn        the function to be throttled
 * @param threshold the time delaying each function call (in milliseconds).
 */
sirius.throttle = function (fn, threshold) {
    let last;
    let deferTimer;

    return function () {
        const context = this;
        const now = Date.now();
        const args = arguments;

        if (last && now < last + threshold) {
            // hold on to it
            clearTimeout(deferTimer);

            deferTimer = setTimeout(function () {
                last = now;
                fn.apply(context, args);
            }, threshold + last - now);
        } else {
            last = now;
            fn.apply(context, args);
        }
    };
}

/**
 * Adds an event listener that triggers exactly once per element when it becomes visible or is about to become visible.
 * <p>
 * The event will trigger once an element comes within a certain distance to the viewport. By default the vertical and
 * horizontal distances are equal to the element's height and width respectively. This distance can be increased by
 * specifying a value greater than 1 as the distanceFactor.
 *
 * @param selector       the selector the elements need to match
 * @param listener       the function to call for every element
 * @param distanceFactor the optional distance factor to control how far away the event will trigger, 1 by default
 */
sirius.addElementVisibleListener = function (selector, listener, distanceFactor) {
    const isElementNowOrSoonVisible = function (element) {
        const factor = distanceFactor || 1;
        const rect = element.getBoundingClientRect();

        const verticalMargin = rect.height * factor;
        const horizontalMargin = rect.width * factor;
        const clientHeight = document.documentElement.clientHeight;
        const clientWidth = document.documentElement.clientWidth;

        const visibleVertically = (rect.bottom + verticalMargin) >= 0 && (rect.top - verticalMargin) < clientHeight;
        const visibleHorizontally = (rect.left + horizontalMargin) >= 0 && (rect.right - horizontalMargin) < clientWidth;

        return visibleVertically && visibleHorizontally;
    };

    const handleVisibleElements = function () {
        const elements = document.querySelectorAll(selector);

        for (let i = 0; i < elements.length; i++) {
            let element = elements[i];

            if (!element.getAttribute('data-scroll-event-handled') && isElementNowOrSoonVisible(element)) {
                element.setAttribute('data-scroll-event-handled', true);

                listener(element);
            }
        }
    };

    // Check after the page is loaded and when resizing or scrolling the page
    sirius.ready(handleVisibleElements);
    window.addEventListener('resize', sirius.throttle(handleVisibleElements, 20));
    document.addEventListener('scroll', sirius.throttle(handleVisibleElements, 20));

    // Listen for DOM changes that might influence the position of other elements
    const observer = new MutationObserver(sirius.throttle(handleVisibleElements, 20));

    observer.observe(document, {
        childList: true,
        subtree: true,
        attributes: true,
        characterData: true
    });
}
