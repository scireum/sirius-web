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
        var _window = $(window);
        _touchTarget.off('mousedown.dragMe').on('mousedown.dragMe', function (e) {
            var offset = $(this).offset();
            //mouse position relative to div
            var x = e.pageX - offset.left;
            var y = e.pageY - offset.top;
            _window.on('mousemove.dragMe', function (event) {
                var containerOffset = _container.offset();
                var maxBottom = containerOffset.top + _container.outerHeight() - _div.outerHeight();
                var maxRight = containerOffset.left + _container.outerWidth() - _div.outerWidth();
                var mouseY = event.clientY - y + _container.scrollTop();
                var mouseX = event.clientX - x + _container.scrollLeft()
                var top = Math.min(Math.max(containerOffset.top, mouseY), maxBottom);
                var left = Math.min(Math.max(containerOffset.left, mouseX), maxRight);
                _div.css({
                    position: 'absolute',
                    margin: '',
                    top: top + 'px',
                    left: left + 'px'
                });
            });
        });
        _window.on('mouseup.dragMe', function (e) {
            _window.off('mousemove.dragMe');
        });
    }
}(jQuery));