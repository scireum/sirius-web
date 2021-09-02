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
        let urls = JSON.parse(window.sessionStorage.getItem("tycho-history"));
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
        while (urls.length > 25) {
            urls.shift()
        }

        window.sessionStorage.setItem("tycho-history", JSON.stringify(urls));
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

    if (window.history.state && window.history.state.url === url) {
        // this site was restored from a state
        if (url === window.sessionStorage.getItem('tycho-history-skip-url')) {
            // we want to skip this state
           window.history.back();
        } else {
            window.sessionStorage.removeItem('tycho-history-skip-url')
        }
        // search in local storage history for the url and rewind history to that stage
        const lastIndexOf = urls.lastIndexOf(url);
        if (lastIndexOf === -1) {
            // Not in our history, probably 'forward' navigation make it the new 'newest'
            urls.push(url);
            // Also append the same entry again so we erase further forward navigation, as it will fail anyway
            window.history.pushState({url: url}, document.title, window.location.href);
            // As we duplicated the state, we need to skip the next back navigation
            window.sessionStorage.setItem('tycho-history-skip-url', url);
        } else {
            // 'back' navigation, remove everything behind this url
            urls.splice(lastIndexOf + 1);
        }
        storeTychoHistory(urls);
        return;
    }

    if (urls.length !== 0 && urls[urls.length - 1] === url) {
        // If the user navigates back from this state, we need to skip the entries with the same url
        window.sessionStorage.setItem('tycho-history-skip-url', url);
        // We also want to skip it, if this state is called..
        url = '-';
    }

    if (url !== '-') {
        urls.push(url);
        storeTychoHistory(urls);
    }

    // Store the url to load in the current state
    window.history.replaceState({url: url}, document.title, window.location.href);
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


sirius.ready(function () {
    if (hasHistoryUrls()) {
        document.querySelectorAll('.back-button').forEach(function (_node) {
            _node.style.display = 'inline-block';
            _node.parentNode.classList.remove('d-none');
        });
    }
})

// On page load, if we have a state with a different url, load it
if (window.history.state) {
    const url = window.history.state.url;
    if (url === '-') {
        window.sessionStorage.removeItem('tycho-history-skip-url');
        window.history.back();
    } else if (url !== currentUri()) {
        window.location.href = url;
    }
}
