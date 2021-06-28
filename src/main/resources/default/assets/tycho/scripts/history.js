/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

function currentUri() {
    return location.pathname + location.search;
}

function fetchTychoHistory() {
    try {
        let urls = JSON.parse(window.localStorage.getItem("tycho-history"));
        if (urls == null || typeof urls !== "object") {
            return [];
        } else {
            return urls;
        }
    } catch (e) {
        console.log(e);
        return [];
    }
}

function storeTychoHistory(urls) {
    try {
        while(urls.length > 25) {
            urls.shift()
        }
        
        window.localStorage.setItem("tycho-history", JSON.stringify(urls));
    } catch (e) {
        console.log(e);
    }
}

function appendHistoryUrl(url) {
    // No explicit URL is given, use the current one...
    if (url === '') {
        url = currentUri();
    }
    let urls = fetchTychoHistory();
    if (urls.length === 0 || urls[urls.length - 1] !== url) {
        urls.push(url);
        storeTychoHistory(urls);
    }

    // We just create a fake entry so that a popstate event is created if the back button is pressed...
    window.history.pushState({}, document.title, window.location.href);
}

function hasHistoryUrls() {
    let urls = fetchTychoHistory();
    for (let i = urls.length - 1; i >= 0; i--) {
        if (urls[i] !== currentUri()) {
            return true;
        }
    }
    return false;
}

window.addEventListener("popstate", function (e) {
    let urls = fetchTychoHistory();
    while (urls.length > 0) {
        let url = urls.pop();
        if (url !== currentUri()) {
            window.location.href = url;
            storeTychoHistory(urls);
            return;
        }
    }
    storeTychoHistory(urls);

    // We couldn't go anywhere, use the real history...
    window.history.back();
});

sirius.ready(function() {
    if (hasHistoryUrls()) {
        document.querySelectorAll('.back-button').forEach(function (_node) {
            _node.style.display = 'inline-block';
            _node.parentNode.classList.remove('d-none');
        });
    }

})
