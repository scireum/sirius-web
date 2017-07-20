/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

(function ($) {
    $.fn.dragMe = function (options) {
        if (typeof options === 'undefined') {
            options = {};
        }
        var _div = this;
        var _touchTarget = $(options.touchTarget) || _div;
        var _container = $(options.container || 'body');
        _touchTarget.off('mousedown.dragMe').on('mousedown.dragMe', function (e) {
            var offset = $(this).offset();
            var x = e.pageX - offset.left;
            var y = e.pageY - offset.top;
            $(window).on('mousemove.dragMe', function (event) {
                var containerOffset = _container.offset();
                var maxBottom = containerOffset.top + _container.outerHeight() - _div.outerHeight();
                var maxRight = containerOffset.left + _container.outerWidth() - _div.outerWidth();
                var top = Math.min(Math.max(containerOffset.top, event.clientY - y), maxBottom);
                var left = Math.min(Math.max(containerOffset.left, event.clientX - x), maxRight);
                _div.css({
                    position: 'absolute',
                    margin: '',
                    top: top + 'px',
                    left: left + 'px'
                });
            });
        });
        $(window).on('mouseup.dragMe', function (e) {
            $(window).off('mousemove.dragMe');
        });
    }
}(jQuery));