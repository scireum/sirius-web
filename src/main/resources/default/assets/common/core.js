// Checks if the given string is empty or null.
sirius.isEmpty = function (aString) {
    return aString === null || aString === undefined || aString === '';
}

// Checks if the given string is non-empty.
sirius.isFilled = function (aString) {
    return aString !== null && aString !== undefined && aString !== '';
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
