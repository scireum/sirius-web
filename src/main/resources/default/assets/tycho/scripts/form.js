sirius.ready(function () {
    document.querySelectorAll('.single-click-link').forEach(function (_node) {
        _node.addEventListener('click', function (e) {
            if (_node.classList.contains('single-click-pending')) {
                e.preventDefault();
                e.cancelBubble = true;
                return true;
            }
            _node.classList.add('single-click-pending');
        });
    });

    document.querySelectorAll('.submit-link').forEach(function (_node) {
        _node.addEventListener('click', function () {
            let _form = sirius.findParentOfType(_node, 'FORM');
            if (_form != null) {
                _form.submit();
            }
        });
    });
});
