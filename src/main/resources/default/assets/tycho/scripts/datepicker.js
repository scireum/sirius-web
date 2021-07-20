sirius.ready(function() {
    $('.date-picker').datetimepicker({
        locale: tycho_current_locale,
        ignoreReadonly: true,
        keepInvalid: true,
        useCurrent: false,
        showTodayButton: true,
        icons: {
            time: 'fa fa-clock-o',
            date: 'fa fa-calendar',
            up: 'fa fa-chevron-up',
            down: 'fa fa-chevron-down',
            previous: 'fa fa-chevron-left',
            next: 'fa fa-chevron-right',
            today: 'fa fa-history',
            clear: 'fa fa-trash',
            close: 'fa fa-remove'
        }
    });
});
