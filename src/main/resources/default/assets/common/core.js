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
    _node.addEventListener('keyup', function(event) {
        if (event.code === 'Enter') {
            event.preventDefault();
            listener();
        }
    });
}
