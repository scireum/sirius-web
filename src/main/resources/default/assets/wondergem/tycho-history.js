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

function appendHistoryUrl() {

    // on wondergem, always remove potential tycho skips
    window.sessionStorage.removeItem('tycho-history-skip-url')

    // No explicit URL is given, use the current one...
    let url = currentUri();

    let urls = fetchTychoHistory();

    if (window.history.state && window.history.state.url === url) {
        // this site was restored from a state
        // search in local storage history for the url and rewind history to that stage
        const lastIndexOf = urls.lastIndexOf(url);
        if (lastIndexOf === -1) {
            // Not in our history, probably 'forward' navigation make it the new 'newest'
            urls.push(url);
        } else {
            // 'back' navigation, remove everything behind this url
            urls.splice(lastIndexOf + 1);
        }
        storeTychoHistory(urls);
        return;
    }

    urls.push(url);
    storeTychoHistory(urls);


    // Store the url to load in the current state
    window.history.replaceState({url: url}, document.title, window.location.href);
}

