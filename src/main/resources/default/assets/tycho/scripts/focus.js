/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

sirius.ready(function() {

    function focus(field) {
        field.focus();

        if (typeof field.selectionStart == "number") {
            field.selectionStart = field.selectionEnd = field.value.length;
        } else if (typeof field.createTextRange != "undefined") {
            field.focus();
            let range = el.createTextRange();
            range.collapse(false);
            range.select();
        }
    }

    let primaryField = document.querySelector('.primary-autofocus');
    if (primaryField !== null) {
        focus(primaryField);

        return;
    }
    let autofocusField = document.querySelector('.autofocus');
    if (autofocusField !== null) {
        focus(autofocusField);
        return;
    }

    document.querySelectorAll('.submit-on-enter').forEach(function(field) {
       sirius.addEnterListener(field, function() {
           let form = sirius.findParentOfType(field,'form');
           if (form !== null) {
               form.submit();
           }
       });
    });
});
