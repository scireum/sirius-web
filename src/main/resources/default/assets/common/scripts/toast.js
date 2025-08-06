window.sirius.toast = (function () {

    /**
     * Represents a singular toast notification that will be displayed on the screen.
     */
    class Toast {
        static defaults = {
            message: '', // Default message to be displayed in the toast
            type: 'info', // Default toast type
            closable: true, // Whether the toast can be closed by the user
            animation: true, // Whether to animate the toast appearance
        }

        static template = `
            <div class="sci-card sci-shadow-elevated sci-position-relative sci-border-radius-75 sci-p-2 sci-pl-3 sci-toast sci-d-flex sci-align-items-center sci-justify-content-space-between"
                 role="alert"
                 data-toast-type="{{type}}">
                {{{message}}}
                {{#closable}}
                    <div role="button" class="sci-icon-smaller sci-icon-close sci-ms-2 sci-cursor-pointer sci-toast-button-close sci-toast-button-close-js"></div>
                {{/closable}}
            </div>`;

        constructor(manager, options = {}) {
            this.manager = manager;
            this.options = sirius.deepExtend(Toast.defaults, options);
            this.#show();
        }

        #show() {
            this._toast = document.createElement('div');
            this._toast.classList.add('sci-toast-wrapper');
            if (this.options.animation) {
                this._toast.classList.add('sci-toast-animated');
            }

            const _html = Mustache.render(Toast.template, this.options);
            this._toast.innerHTML = _html;

            this.manager._toastContainer.appendChild(this._toast);
            this._toast.toast = this;

            if (this.options.duration > 0) {
                this.#scheduleHide();

                // Keep on screen when hovered or focused
                this._toast.addEventListener('mouseover', event => this.#handleInteraction(event));
                this._toast.addEventListener('mouseout', event => this.#handleInteraction(event));
                this._toast.addEventListener('focusin', event => this.#handleInteraction(event));
                this._toast.addEventListener('focusout', event => this.#handleInteraction(event));
            }

            if (this.options.closable) {
                const _closeButton = this._toast.querySelector('.sci-toast-button-close-js');
                if (_closeButton) {
                    _closeButton.addEventListener('click', () => this.hide());
                }
            }
        }

        #handleInteraction(event) {
            if (event.type === 'mouseover' || event.type === 'focusin') {
                // Stop the timer to remove the toast
                clearTimeout(this.hideTimeout);
            } else if (event.type === 'mouseout' || event.type === 'focusout') {
                // Restart the timer to remove the toast after the specified duration
                this.#scheduleHide();
            }
        }

        #scheduleHide() {
            this.hideTimeout = setTimeout(() => this.hide(), this.options.duration);
        }

        /**
         * Hides the toast from the screen.
         *
         * If animation is enabled, it will animate the disappearance, otherwise it will be removed immediately.
         */
        hide() {
            if (!this.options.animation) {
                this.#remove();
                return;
            }

            this._toast.addEventListener('transitionend', () => this.#remove(), {once: true});
            this._toast.style.maxHeight = 0;
            this._toast.style.opacity = 0;
        }

        #remove() {
            if (!this._toast) {
                return;
            }
            this.manager._toastContainer.removeChild(this._toast);
            this._toast = null;
            clearTimeout(this.hideTimeout);
            this.hideTimeout = null;
        }
    }

    /**
     * Responsible for managing and rendering toasts on the screen.
     * It provides methods to configure the toast manager and create toasts with various options.
     */
    class ToastManager {
        configuration = {
            position: 'top-right', // Default position of the toast container
            duration: 3000, // Default duration for toast visibility (can be overridden in individual toasts)
        };

        constructor() {
        }

        /**
         * Configures the ToastManager with the given configuration options.
         *
         * @param {object} configuration the configuration options to override the defaults
         * @param {string} [configuration.position=top-right] the position of the toast container, a combination of 'top' or 'bottom' and 'left', 'right' or 'center' (e.g., 'top-right', 'bottom-left').
         */
        configure(configuration = {}) {
            this.configuration = sirius.deepExtend(this.configuration, configuration);
            this.#createContainer();
        }

        /**
         * Creates and shows a toast with the given options.
         *
         * @param options {object} the options for the toast
         * @param {string} [options.message=''] the message to display in the toast
         * @param {string} [options.type='info'] the type of the toast, can be 'info', 'success', 'warning', or 'error'
         * @param {number} [options.duration=3000] the duration in milliseconds for which the toast should be visible, 0 means it will not disappear automatically
         * @param {boolean} [options.closable=true] whether the toast can be closed by the user
         * @param {boolean} [options.animation=true] whether to animate the toast appearance and disappearance
         * @returns {Toast} the created Toast instance
         */
        show(options = Toast.defaults) {
            if (!this._toastContainer) {
                this.#createContainer();
            }

            const effectiveOptions = sirius.deepExtend({
                duration: this.configuration.duration,
            }, options);
            return new Toast(this, effectiveOptions);
        }

        #createContainer() {
            if (this._toastContainer) {
                document.body.removeChild(this._toastContainer);
            }
            this._toastContainer = document.createElement('div');
            this._toastContainer.id = 'sci-toasts-container';
            this._toastContainer.classList.add('sci-d-flex', 'sci-flex-column', 'sci-position-absolute', 'sci-font', 'sci-text', 'sci-text-soft');
            this._toastContainer.dataset.toastPosition = this.configuration.position;
            document.body.appendChild(this._toastContainer);
            this._toastContainer.toastManager = this;
        }

    }

    return new ToastManager();
})();
