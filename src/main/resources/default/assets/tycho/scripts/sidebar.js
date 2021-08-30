sirius.ready(function() {
    let _menuButton = document.querySelector('.page-header .sidebar-button-js');
    let _sidebarDiv = document.querySelector('.sidebar-js');

    if (_menuButton !== null && _sidebarDiv !== null) {
        _menuButton.classList.remove("d-none");
        _menuButton.classList.add("d-xs-block");
        _menuButton.addEventListener('click', function() {
            if (_menuButton.classList.contains('active')) {
                _sidebarDiv.classList.add('d-none');
                _menuButton.classList.remove('active');
            } else {
                _sidebarDiv.classList.remove('d-none');
                _menuButton.classList.add('active');
            }
        });
    }
});
