/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

sirius.ready(function() {

    function focus(_field) {
        _field.focus();

        if (typeof _field.selectionStart == "number") {
            _field.selectionStart = _field.selectionEnd = _field.value.length;
        } else if (typeof _field.createTextRange != "undefined") {
            _field.focus();
            let range = el.createTextRange();
            range.collapse(false);
            range.select();
        }
    }

    let _primaryField = document.querySelector('.primary-autofocus');
    if (_primaryField !== null) {
        focus(_primaryField);
    } else {
        let _autofocusField = document.querySelector('.autofocus');
        if (_autofocusField !== null) {
            focus(_autofocusField);
        }
    }

    document.querySelectorAll('.submit-on-enter').forEach(function(_field) {
       sirius.addEnterListener(_field, function() {
           let _form = sirius.findParentOfType(_field,'FORM');
           if (_form !== null) {
               _form.submit();
           }
       });
    });
});
