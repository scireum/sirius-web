// Checks if the given string is empty or null.
sirius.isEmpty = function (aString) {
    return aString === null || aString === undefined || aString === '';
}

// Checks if the given string is non-empty.
sirius.isFilled = function (aString) {
    return aString !== null && aString !== undefined && aString !== '';
}

