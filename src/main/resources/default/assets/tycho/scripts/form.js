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

    const _modalElement = document.getElementById('link-confirm-modal');
    const _confirmForm = _modalElement.querySelector(".confirm-form-js");
    const _submitBtn = _modalElement.querySelector("button[type='submit']");
    _submitBtn.addEventListener('click', function () {
        _confirmForm.submit();
    });

    document.querySelectorAll('.confirm-link-js').forEach(function (_node) {
        _node.addEventListener('click', function (event) {
            try {
                _confirmForm.setAttribute('action', _node.getAttribute('href'));

                $('#link-confirm-modal').modal({
                    keyboard: true
                }).on('shown', function () {
                    _modalElement.querySelector('.btn-close').focus();
                }).attr('tabindex', -1);
            } catch (e) {
                console.log(e);
            }

            event.preventDefault();
            return false;
        });
    });
});

