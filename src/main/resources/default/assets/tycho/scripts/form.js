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

    document.querySelectorAll('.danger-link').forEach(function (_node) {
        _node.addEventListener('click', function () {
            if (_node.matches('.guarded-link')) {
                try {
                    let modalElement = document.getElementById('danger-link-confirm');
                    let submitBtn = modalElement.querySelector("button[type='submit']");

                    submitBtn.addEventListener('click', function() {
                        document.getElementById(_node.getAttribute('data-deleteid')).submit();
                    });

                    $('#danger-link-confirm').modal({
                        keyboard: true
                    }).on('shown', function () {
                        modalElement.querySelector('.btn-close').focus();
                    }).attr('tabindex', -1);
                } catch (e) {
                    console.log(e);
                }

                return false;
            } else {
                try {
                    let modalElement = document.getElementById('danger-link-confirm');

                    let okayLink = _node.getAttribute('href');
                    let submitBtn = modalElement.querySelector("button[type='submit']");

                    submitBtn.addEventListener('click', function() {
                        window.location.href = okayLink;
                    });

                    $('#danger-link-confirm').modal({
                        keyboard: true
                    }).on('shown', function () {
                        modalElement.querySelector('.btn-close').focus();
                    }).setAttribute('tabindex', -1);
                } catch (e) {
                    console.log(e);
                }

                return false;
            }
        });
    });
});

