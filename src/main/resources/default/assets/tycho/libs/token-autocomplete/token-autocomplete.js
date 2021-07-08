var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var SelectModes;
(function (SelectModes) {
    SelectModes[SelectModes["SINGLE"] = 0] = "SINGLE";
    SelectModes[SelectModes["MULTI"] = 1] = "MULTI";
    SelectModes[SelectModes["SEARCH"] = 2] = "SEARCH";
})(SelectModes || (SelectModes = {}));
var TokenAutocomplete = /** @class */ (function () {
    function TokenAutocomplete(options) {
        this.KEY_BACKSPACE = 'Backspace';
        this.KEY_ENTER = 'Enter';
        this.KEY_TAB = 'Tab';
        this.KEY_UP = 'ArrowUp';
        this.KEY_DOWN = 'ArrowDown';
        this.KEY_LEFT = 'ArrowLeft';
        this.KEY_RIGHT = 'ArrowRight';
        this.KEY_ESC = 'Escape';
        this.defaults = {
            name: '',
            selector: '',
            noMatchesText: null,
            placeholderText: 'enter some text',
            initialTokens: null,
            initialSuggestions: null,
            tokenRenderer: TokenAutocomplete.MultiSelect.defaultRenderer,
            suggestionsUri: '',
            selectMode: SelectModes.MULTI,
            suggestionsUriBuilder: function (query) {
                return this.suggestionsUri + '?query=' + query;
            },
            suggestionRenderer: TokenAutocomplete.Autocomplete.defaultRenderer,
            minCharactersForSuggestion: 1,
            allowCustomEntries: true,
            readonly: false,
            optional: false,
            enableTabulator: true
        };
        this.options = __assign(__assign({}, this.defaults), options);
        var passedContainer = document.querySelector(this.options.selector);
        if (!passedContainer) {
            throw new Error('passed selector does not point to a DOM element.');
        }
        this.container = passedContainer;
        this.container.classList.add('token-autocomplete-container');
        if (!Array.isArray(this.options.initialTokens) && !Array.isArray(this.options.initialSuggestions)) {
            this.parseTokensAndSuggestions();
        }
        this.hiddenSelect = document.createElement('select');
        this.hiddenSelect.id = this.container.id + '-select';
        this.hiddenSelect.name = this.options.name;
        this.hiddenSelect.setAttribute('multiple', 'true');
        this.hiddenSelect.style.display = 'none';
        if (this.options.readonly && this.options.tokenRenderer === TokenAutocomplete.MultiSelect.defaultRenderer) {
            this.options.tokenRenderer = TokenAutocomplete.MultiSelect.defaultReadonlyRenderer;
        }
        this.textInput = document.createElement('span');
        this.textInput.id = this.container.id + '-input';
        this.textInput.classList.add('token-autocomplete-input');
        if (!this.options.readonly) {
            if (this.options.placeholderText != null) {
                this.textInput.dataset.placeholder = this.options.placeholderText;
            }
            this.textInput.contentEditable = 'true';
            this.textInput.addEventListener("paste", function (event) {
                var _a;
                event.preventDefault();
                var text = (_a = event.clipboardData) === null || _a === void 0 ? void 0 : _a.getData("text/plain");
                document.execCommand("insertHTML", false, text);
            });
        }
        else {
            this.container.classList.add('token-autocomplete-readonly');
        }
        this.container.appendChild(this.textInput);
        this.container.appendChild(this.hiddenSelect);
        this.addHiddenEmptyOption();
        if (this.options.selectMode == SelectModes.MULTI) {
            this.select = new TokenAutocomplete.MultiSelect(this);
        }
        else if (this.options.selectMode == SelectModes.SEARCH) {
            this.select = new TokenAutocomplete.SearchMultiSelect(this);
        }
        else if (this.options.selectMode == SelectModes.SINGLE) {
            this.hiddenSelect.removeAttribute('multiple');
            this.select = new TokenAutocomplete.SingleSelect(this);
        }
        this.autocomplete = new TokenAutocomplete.Autocomplete(this);
        this.select.initEventListeners();
        this.autocomplete.initEventListeners();
        this.debug(false);
        if (Array.isArray(this.options.initialTokens)) {
            this.val(this.options.initialTokens);
        }
        this.container.tokenAutocomplete = this;
    }
    /**
     * Searches the element given as a container for option elements and creates active tokens (when the option is marked selected)
     * and suggestions (all options found) from these. During this all found options are removed from the DOM.
     */
    TokenAutocomplete.prototype.parseTokensAndSuggestions = function () {
        var initialTokens = [];
        var initialSuggestions = [];
        var options = this.container.querySelectorAll('option');
        var me = this;
        options.forEach(function (option) {
            if (option.text != null) {
                if (option.hasAttribute('selected')) {
                    initialTokens.push({ value: option.value, text: option.text, type: null });
                }
                initialSuggestions.push({
                    id: null,
                    value: option.value,
                    fieldLabel: option.text,
                    type: null,
                    completionDescription: null,
                    completionLabel: null
                });
            }
            me.container.removeChild(option);
        });
        if (initialTokens.length > 0) {
            this.options.initialTokens = initialTokens;
        }
        if (initialSuggestions.length > 0) {
            this.options.initialSuggestions = initialSuggestions;
        }
    };
    /**
     * Clears the currently present tokens and creates new ones from the given input value.
     *
     * @param {(Array<Token>|string)} value - either the name of a single token or a list of tokens to create
     * @param {boolean} silent - whether appropriate events should be triggered when changing tokens or not
     */
    TokenAutocomplete.prototype.val = function (value, silent) {
        if (silent === void 0) { silent = false; }
        this.select.clear(silent);
        if (Array.isArray(value)) {
            var me_1 = this;
            value.forEach(function (token) {
                if (typeof token === 'object') {
                    me_1.select.addToken(token.value, token.text, token.type, silent);
                }
            });
        }
        else {
            this.select.addToken(value.value, value.text, value.type, silent);
        }
    };
    /**
     * Returns the current text the user has input which is not converted into a token.
     */
    TokenAutocomplete.prototype.getCurrentInput = function () {
        return this.textInput.textContent || '';
    };
    TokenAutocomplete.prototype.setCurrentInput = function (input, silent) {
        this.textInput.textContent = input;
        if (silent) {
            return;
        }
        this.container.dispatchEvent(new CustomEvent('query-changed', {
            detail: {
                query: input
            }
        }));
    };
    TokenAutocomplete.prototype.addHiddenOption = function (tokenValue, tokenText, tokenType) {
        var _a;
        (_a = this.hiddenSelect.querySelector('.empty-token')) === null || _a === void 0 ? void 0 : _a.remove();
        var option = document.createElement('option');
        option.text = tokenText;
        option.value = tokenValue;
        option.setAttribute('selected', 'true');
        option.dataset.text = tokenText;
        option.dataset.value = tokenValue;
        if (tokenType != null) {
            option.dataset.type = tokenType;
        }
        this.hiddenSelect.add(option);
    };
    TokenAutocomplete.prototype.addHiddenEmptyOption = function () {
        var option = document.createElement('option');
        option.text = '';
        option.value = '';
        option.setAttribute('selected', 'true');
        option.classList.add('empty-token');
        this.hiddenSelect.add(option);
    };
    TokenAutocomplete.prototype.setPlaceholderText = function (placeholderText) {
        this.textInput.dataset.placeholder = placeholderText;
    };
    TokenAutocomplete.prototype.debug = function (state) {
        if (state) {
            this.log = console.log.bind(window.console);
        }
        else {
            this.log = function () {
            };
        }
    };
    var _a, _b;
    TokenAutocomplete.MultiSelect = (_a = /** @class */ (function () {
            function class_1(parent) {
                this.parent = parent;
                this.container = parent.container;
                this.options = parent.options;
                this.renderer = parent.options.tokenRenderer;
            }
            class_1.prototype.clearCurrentInput = function () {
                this.parent.textInput.textContent = '';
            };
            class_1.prototype.initEventListeners = function () {
                var me = this;
                var parent = this.parent;
                if (parent.options.readonly) {
                    return;
                }
                parent.textInput.addEventListener('keydown', function (event) {
                    if (event.key == parent.KEY_ENTER || (event.key == parent.KEY_TAB && parent.options.enableTabulator)) {
                        event.preventDefault();
                        var highlightedSuggestion = parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                        if (parent.options.enableTabulator && highlightedSuggestion == null && event.key == parent.KEY_TAB && parent.autocomplete.areSuggestionsDisplayed()) {
                            highlightedSuggestion = parent.autocomplete.suggestions.firstChild;
                        }
                        if (highlightedSuggestion !== null) {
                            me.clearCurrentInput();
                            if (highlightedSuggestion.classList.contains('token-autocomplete-suggestion-active')) {
                                me.removeTokenWithText(highlightedSuggestion.dataset.tokenText);
                            }
                            else {
                                me.addToken(highlightedSuggestion.dataset.value, highlightedSuggestion.dataset.tokenText, highlightedSuggestion.dataset.type, false);
                            }
                            parent.autocomplete.hideSuggestions();
                        }
                        else {
                            me.handleInputAsValue(parent.getCurrentInput());
                        }
                    }
                    else if (parent.getCurrentInput() === '' && event.key == parent.KEY_BACKSPACE) {
                        event.preventDefault();
                        me.removeLastToken();
                    }
                    if ((event.key == parent.KEY_DOWN || event.key == parent.KEY_UP) && parent.autocomplete.suggestions.childNodes.length > 0) {
                        event.preventDefault();
                    }
                });
            };
            /**
             * Adds the current user input as a net token and resets the input area so new text can be entered.
             *
             * @param {string} input - the actual input the user entered
             */
            class_1.prototype.handleInputAsValue = function (input) {
                if (this.parent.options.allowCustomEntries) {
                    this.clearCurrentInput();
                    this.addToken(input, input, null);
                    return;
                }
                if (this.parent.autocomplete.suggestions.childNodes.length === 1) {
                    this.parent.autocomplete.suggestions.firstChild.click();
                }
                else {
                    this.clearCurrentInput();
                }
            };
            /**
             * Adds a token with the specified name to the list of currently present tokens displayed to the user and the hidden select.
             *
             * @param {string} tokenValue - the actual value of the token to create
             * @param {string} tokenText - the name of the token to create
             * @param {string} tokenType - the type of the token to create
             * @param {boolean} silent - whether an appropriate event should be triggered
             */
            class_1.prototype.addToken = function (tokenValue, tokenText, tokenType, silent) {
                var _a;
                if (silent === void 0) { silent = false; }
                if (tokenValue === null || tokenText === null || tokenType === '_no_match_') {
                    return;
                }
                this.parent.addHiddenOption(tokenValue, tokenText, tokenType);
                var addedToken = {
                    value: tokenValue,
                    text: tokenText,
                    type: tokenType
                };
                var element = this.renderer(addedToken);
                var me = this;
                (_a = element.querySelector('.token-autocomplete-token-delete')) === null || _a === void 0 ? void 0 : _a.addEventListener('click', function () {
                    me.removeToken(element);
                });
                this.container.insertBefore(element, this.parent.textInput);
                if (!silent) {
                    this.container.dispatchEvent(new CustomEvent('tokens-changed', {
                        detail: {
                            tokens: this.currentTokens(),
                            added: addedToken
                        }
                    }));
                }
                this.parent.log('added token', addedToken);
            };
            /**
             * Completely clears the currently present tokens from the field.
             */
            class_1.prototype.clear = function (silent) {
                if (silent === void 0) { silent = false; }
                var tokens = this.container.querySelectorAll('.token-autocomplete-token');
                var me = this;
                tokens.forEach(function (token) {
                    me.removeToken(token, silent);
                });
            };
            /**
             * Removes the last token in the list of currently present token. This is the last added token next to the input field.
             */
            class_1.prototype.removeLastToken = function () {
                var tokens = this.container.querySelectorAll('.token-autocomplete-token');
                var token = tokens[tokens.length - 1];
                if (token) {
                    this.removeToken(token);
                }
            };
            /**
             * Removes the specified token from the list of currently present tokens.
             *
             * @param {Element} token - the token to remove
             * @param {boolean} silent - whether an appropriate event should be triggered
             */
            class_1.prototype.removeToken = function (token, silent) {
                var _a;
                if (silent === void 0) { silent = false; }
                this.container.removeChild(token);
                var tokenText = token.dataset.text;
                var hiddenOption = this.parent.hiddenSelect.querySelector('option[data-text="' + tokenText + '"]');
                (_a = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.parentElement) === null || _a === void 0 ? void 0 : _a.removeChild(hiddenOption);
                var addedToken = {
                    value: token.dataset.value,
                    text: tokenText,
                    type: token.dataset.type
                };
                if (!silent) {
                    this.container.dispatchEvent(new CustomEvent('tokens-changed', {
                        detail: {
                            tokens: this.currentTokens(),
                            removed: addedToken
                        }
                    }));
                }
                if (this.currentTokens().length === 0) {
                    this.parent.addHiddenEmptyOption();
                }
                this.parent.log('removed token', token.textContent);
            };
            class_1.prototype.removeTokenWithText = function (tokenText) {
                if (tokenText === null) {
                    return;
                }
                var token = this.container.querySelector('.token-autocomplete-token[data-text="' + tokenText + '"]');
                if (token !== null) {
                    this.removeToken(token);
                }
            };
            class_1.prototype.currentTokens = function () {
                var tokens = [];
                this.parent.hiddenSelect.querySelectorAll('option').forEach(function (option) {
                    if (option.dataset.value != null) {
                        tokens.push(option.dataset.value);
                    }
                });
                return tokens;
            };
            return class_1;
        }()),
        _a.defaultRenderer = function (token) {
            var chip = document.createElement('span');
            chip.classList.add('token-autocomplete-token');
            chip.dataset.text = token.text;
            chip.dataset.value = token.value;
            if (token.type != null) {
                chip.dataset.type = token.type;
            }
            chip.textContent = token.text;
            var deleteToken = document.createElement('span');
            deleteToken.classList.add('token-autocomplete-token-delete');
            deleteToken.textContent = '\u00D7';
            chip.appendChild(deleteToken);
            return chip;
        },
        _a.defaultReadonlyRenderer = function (token) {
            var chip = document.createElement('span');
            chip.classList.add('token-autocomplete-token');
            chip.dataset.text = token.text;
            chip.dataset.value = token.value;
            if (token.type != null) {
                chip.dataset.type = token.type;
            }
            chip.textContent = token.text;
            return chip;
        },
        _a);
    TokenAutocomplete.SingleSelect = /** @class */ (function () {
        function class_2(parent) {
            this.parent = parent;
            this.container = parent.container;
            this.options = parent.options;
            this.container.classList.add('token-autocomplete-singleselect');
            this.parent.textInput.tabIndex = 0;
            if (this.options.optional) {
                var deleteToken = document.createElement('span');
                deleteToken.classList.add('token-singleselect-token-delete');
                deleteToken.textContent = '\u00D7';
                this.container.append(deleteToken);
            }
        }
        class_2.prototype.clear = function (silent) {
            var _a;
            if (this.options.readonly) {
                return;
            }
            var me = this;
            var tokenText = me.parent.textInput.textContent;
            var hiddenOption = me.parent.hiddenSelect.querySelector('option[data-text="' + tokenText + '"]');
            if (!me.options.optional) {
                me.previousValue = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.dataset.value;
                me.previousText = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.dataset.text;
                me.previousType = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.dataset.type;
            }
            else {
                this.container.classList.remove('optional-singleselect-with-value');
            }
            (_a = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.parentElement) === null || _a === void 0 ? void 0 : _a.removeChild(hiddenOption);
            me.parent.addHiddenEmptyOption();
            me.parent.textInput.textContent = '';
            me.parent.textInput.contentEditable = 'true';
        };
        /**
         * Adds the current user input as a net token and resets the input area so new text can be entered.
         *
         * @param {string} input - the actual input the user entered
         */
        class_2.prototype.handleInputAsValue = function (input) {
            if (this.parent.autocomplete.suggestions.childNodes.length === 1) {
                this.parent.autocomplete.suggestions.firstChild.click();
            }
            else {
                this.clearCurrentInput();
            }
        };
        class_2.prototype.currentTokens = function () {
            var tokens = [];
            this.parent.hiddenSelect.querySelectorAll('option').forEach(function (option) {
                if (option.dataset.value != null) {
                    tokens.push(option.dataset.value);
                }
            });
            return tokens;
        };
        class_2.prototype.clearCurrentInput = function () {
            this.clear(true);
        };
        class_2.prototype.addToken = function (tokenValue, tokenText, tokenType, silent) {
            if (tokenValue === null || tokenText === null || tokenType === '_no_match_') {
                return;
            }
            this.clear(true);
            this.parent.textInput.textContent = tokenText;
            this.parent.textInput.contentEditable = 'false';
            if (this.options.optional && tokenText !== '') {
                this.container.classList.add('optional-singleselect-with-value');
            }
            this.parent.addHiddenOption(tokenValue, tokenText, tokenType);
        };
        class_2.prototype.initEventListeners = function () {
            var _a;
            var me = this;
            var parent = this.parent;
            if (parent.options.readonly) {
                return;
            }
            parent.textInput.addEventListener('keydown', function (event) {
                if (event.key == parent.KEY_ENTER || (event.key == parent.KEY_TAB && parent.options.enableTabulator)) {
                    event.preventDefault();
                    var highlightedSuggestion = parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                    if (parent.options.enableTabulator && highlightedSuggestion == null && event.key == parent.KEY_TAB && parent.autocomplete.areSuggestionsDisplayed()) {
                        highlightedSuggestion = parent.autocomplete.suggestions.firstChild;
                    }
                    if (highlightedSuggestion !== null) {
                        me.addToken(highlightedSuggestion.dataset.value, highlightedSuggestion.dataset.tokenText, highlightedSuggestion.dataset.type, false);
                        parent.autocomplete.hideSuggestions();
                    }
                    else {
                        me.handleInputAsValue(parent.getCurrentInput());
                    }
                }
                if ((event.key == parent.KEY_DOWN || event.key == parent.KEY_UP) && parent.autocomplete.suggestions.childNodes.length > 0) {
                    event.preventDefault();
                }
            });
            parent.textInput.addEventListener('click', function (event) {
                if (!parent.autocomplete.areSuggestionsDisplayed()) {
                    parent.textInput.focus();
                }
            });
            me.parent.textInput.addEventListener('focusin', function (event) {
                if (!parent.autocomplete.areSuggestionsDisplayed()) {
                    parent.autocomplete.showSuggestions();
                    parent.autocomplete.loadSuggestions();
                }
                // move the cursor into the editable div
                var selection = window.getSelection();
                var range = document.createRange();
                selection === null || selection === void 0 ? void 0 : selection.removeAllRanges();
                range.selectNodeContents(parent.textInput);
                range.collapse(false);
                selection === null || selection === void 0 ? void 0 : selection.addRange(range);
                parent.textInput.focus();
            });
            parent.textInput.addEventListener('focusout', function (event) {
                // we use setTimeout here so we won't interfere with a user clicking on a suggestion
                setTimeout(function () {
                    if (!me.options.optional && (me.currentTokens().length === 0 || me.currentTokens()[0] === '')) {
                        me.addToken(me.previousValue, me.previousText, me.previousType, true);
                    }
                }, 200);
            });
            (_a = parent.container.querySelector('.token-singleselect-token-delete')) === null || _a === void 0 ? void 0 : _a.addEventListener('click', function () {
                me.clear(false);
            });
        };
        return class_2;
    }());
    TokenAutocomplete.SearchMultiSelect = /** @class */ (function (_super) {
        __extends(class_3, _super);
        function class_3() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        /**
         * Instead of adding the custom user input as a token and handling it as a filter we let it remain in the input
         * area and instead send an event so the user search request can be handled / executed.
         *
         * @param {string} input - the actual input the user entered
         */
        class_3.prototype.handleInputAsValue = function (input) {
            this.container.dispatchEvent(new CustomEvent('query-changed', {
                detail: {
                    query: input
                }
            }));
        };
        return class_3;
    }(TokenAutocomplete.MultiSelect));
    TokenAutocomplete.Autocomplete = (_b = /** @class */ (function () {
            function class_4(parent) {
                this.parent = parent;
                this.container = parent.container;
                this.options = parent.options;
                this.renderer = parent.options.suggestionRenderer;
                this.suggestions = document.createElement('ul');
                this.suggestions.id = this.container.id + '-suggestions';
                this.suggestions.classList.add('token-autocomplete-suggestions');
                this.container.appendChild(this.suggestions);
            }
            class_4.prototype.initEventListeners = function () {
                var me = this;
                if (me.parent.options.readonly) {
                    return;
                }
                me.parent.textInput.addEventListener('keyup', function (event) {
                    if (event.key == me.parent.KEY_ESC) {
                        me.hideSuggestions();
                        me.parent.textInput.blur();
                        return;
                    }
                    if (event.key == me.parent.KEY_UP && me.suggestions.childNodes.length > 0) {
                        event.preventDefault();
                        var highlightedSuggestion = me.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                        if (highlightedSuggestion == null) {
                            // highlight last entry and scroll to bottom
                            me.highlightSuggestionAtPosition(me.suggestions.childNodes.length - 1);
                            me.suggestions.scrollTop = me.suggestions.scrollHeight;
                            return;
                        }
                        var aboveSuggestion = highlightedSuggestion.previousSibling;
                        if (aboveSuggestion != null) {
                            // if the suggestions is above the scroll position, scroll to the suggestion
                            var suggestionTop = aboveSuggestion.offsetTop;
                            if (me.suggestions.scrollTop > suggestionTop) {
                                me.suggestions.scrollTop = suggestionTop;
                            }
                            me.highlightSuggestion(aboveSuggestion);
                        }
                        else {
                            highlightedSuggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                        }
                        return;
                    }
                    if (event.key == me.parent.KEY_DOWN && me.suggestions.childNodes.length > 0) {
                        event.preventDefault();
                        var highlightedSuggestion = me.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                        if (highlightedSuggestion == null) {
                            // highlight first entry and scroll to top
                            me.highlightSuggestionAtPosition(0);
                            me.suggestions.scrollTop = 0;
                            return;
                        }
                        var belowSuggestion = highlightedSuggestion === null || highlightedSuggestion === void 0 ? void 0 : highlightedSuggestion.nextSibling;
                        if (belowSuggestion != null) {
                            // if the suggestions is not completely visible, scroll until the suggestion is at the bottom
                            var suggestionBottom = belowSuggestion.offsetTop + belowSuggestion.offsetHeight;
                            if (me.suggestions.scrollTop + me.suggestions.clientHeight < suggestionBottom) {
                                me.suggestions.scrollTop = suggestionBottom - me.suggestions.clientHeight;
                            }
                            me.highlightSuggestion(belowSuggestion);
                        }
                        else {
                            highlightedSuggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                        }
                        return;
                    }
                    if (event.key == me.parent.KEY_LEFT || event.key == me.parent.KEY_RIGHT || event.key == me.parent.KEY_ENTER) {
                        // We dont want to retrigger the autocompletion when the user navigates the cursor inside the input.
                        return;
                    }
                    me.loadSuggestions();
                });
                me.parent.textInput.addEventListener('focusout', function (event) {
                    // we use setTimeout here so we won't interfere with a user clicking on a suggestion
                    setTimeout(function () {
                        me.hideSuggestions();
                    }, 200);
                });
                me.parent.textInput.addEventListener('focusin', function (event) {
                    me.loadSuggestions();
                });
            };
            class_4.prototype.loadSuggestions = function () {
                var me = this;
                var value = me.parent.getCurrentInput();
                if (me.parent.options.selectMode == SelectModes.SINGLE) {
                    if (!me.parent.textInput.isContentEditable) {
                        me.parent.select.clear(true);
                    }
                }
                else if (value.length < me.parent.options.minCharactersForSuggestion) {
                    me.hideSuggestions();
                    me.clearSuggestions();
                    return;
                }
                if (me.parent.options.suggestionsUri.length > 0) {
                    me.requestSuggestions(value);
                    return;
                }
                if (Array.isArray(me.parent.options.initialSuggestions)) {
                    me.clearSuggestions();
                    me.parent.options.initialSuggestions.forEach(function (suggestion) {
                        if (typeof suggestion !== 'object') {
                            // the suggestion is of wrong type and therefore ignored
                            return;
                        }
                        var text = suggestion.fieldLabel;
                        if (value.localeCompare(text.slice(0, value.length), undefined, { sensitivity: 'base' }) === 0) {
                            // The suggestion starts with the query text the user entered and will be displayed
                            me.addSuggestion(suggestion);
                        }
                    });
                    if (me.suggestions.childNodes.length == 0 && me.parent.options.noMatchesText) {
                        me.addSuggestion({
                            id: null,
                            value: '_no_match_',
                            fieldLabel: me.parent.options.noMatchesText,
                            type: '_no_match_',
                            completionDescription: null,
                            completionLabel: null
                        });
                    }
                }
            };
            /**
             * Hides the suggestions dropdown from the user.
             */
            class_4.prototype.hideSuggestions = function () {
                this.suggestions.style.display = '';
                var suggestions = this.suggestions.querySelectorAll('li');
                suggestions.forEach(function (suggestion) {
                    suggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                });
            };
            /**
             * Shows the suggestions dropdown to the user.
             */
            class_4.prototype.showSuggestions = function () {
                this.suggestions.style.display = 'block';
            };
            class_4.prototype.areSuggestionsDisplayed = function () {
                return this.suggestions.style.display === 'block';
            };
            class_4.prototype.highlightSuggestionAtPosition = function (index) {
                var suggestions = this.suggestions.querySelectorAll('li');
                suggestions.forEach(function (suggestion) {
                    suggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                });
                suggestions[index].classList.add('token-autocomplete-suggestion-highlighted');
            };
            class_4.prototype.highlightSuggestion = function (suggestion) {
                this.suggestions.querySelectorAll('li').forEach(function (suggestionElement) {
                    suggestionElement.classList.remove('token-autocomplete-suggestion-highlighted');
                });
                suggestion.classList.add('token-autocomplete-suggestion-highlighted');
            };
            /**
             * Removes all previous suggestions from the dropdown.
             */
            class_4.prototype.clearSuggestions = function () {
                this.suggestions.innerHTML = '';
            };
            /**
             * Loads suggestions matching the given query from the rest service behind the URI given as an option while initializing the field.
             *
             * @param query the query to search suggestions for
             */
            class_4.prototype.requestSuggestions = function (query) {
                var me = this;
                if (me.request != null && me.request.readyState) {
                    me.request.abort();
                }
                me.request = new XMLHttpRequest();
                me.request.onload = function () {
                    me.request = null;
                    me.clearSuggestions();
                    var answer = this.response;
                    //IE 11 doesnt properly respect content type header, need to parse json string by hand..
                    if (typeof answer === 'string') {
                        answer = JSON.parse(answer);
                    }
                    if (Array.isArray(answer.completions)) {
                        answer.completions.forEach(function (suggestion) {
                            me.addSuggestion(suggestion);
                        });
                        if (me.suggestions.childNodes.length == 0 && me.options.noMatchesText) {
                            me.addSuggestion({
                                id: null,
                                value: '_no_match_',
                                fieldLabel: me.options.noMatchesText,
                                type: '_no_match_',
                                completionDescription: null,
                                completionLabel: null
                            });
                        }
                    }
                };
                var suggestionsUri = me.options.suggestionsUriBuilder(query);
                me.request.open('GET', suggestionsUri, true);
                me.request.responseType = 'json';
                me.request.setRequestHeader('Content-type', 'application/json');
                me.request.send();
            };
            /**
             * Adds a suggestion with the given text matching the users input to the dropdown.
             *
             * @param {string} suggestion - the metadata of the suggestion that should be added
             */
            class_4.prototype.addSuggestion = function (suggestion) {
                var element = this.renderer(suggestion);
                var value = suggestion.id || suggestion.value;
                var text = suggestion.completionLabel || suggestion.fieldLabel;
                element.dataset.value = value;
                element.dataset.text = text;
                element.dataset.tokenText = suggestion.fieldLabel;
                if (suggestion.type != null) {
                    element.dataset.type = suggestion.type;
                }
                var me = this;
                element.addEventListener('click', function (_event) {
                    if (text == me.options.noMatchesText) {
                        return;
                    }
                    if (me.parent.options.selectMode == SelectModes.SINGLE) {
                        if (element.classList.contains('token-autocomplete-suggestion-active')) {
                            me.parent.select.clear(false);
                        }
                        else {
                            me.parent.select.addToken(value, suggestion.fieldLabel, suggestion.type, false);
                        }
                    }
                    else {
                        me.parent.select.clearCurrentInput();
                        if (element.classList.contains('token-autocomplete-suggestion-active')) {
                            var multiSelect = me.parent.select;
                            multiSelect.removeTokenWithText(text);
                        }
                        else {
                            me.parent.select.addToken(value, suggestion.fieldLabel, suggestion.type, false);
                        }
                    }
                    me.clearSuggestions();
                    me.hideSuggestions();
                });
                if (this.container.querySelector('.token-autocomplete-token[data-value="' + value + '"]') !== null) {
                    element.classList.add('token-autocomplete-suggestion-active');
                }
                this.suggestions.appendChild(element);
                this.showSuggestions();
                me.parent.log('added suggestion', suggestion);
            };
            return class_4;
        }()),
        _b.defaultRenderer = function (suggestion) {
            var option = document.createElement('li');
            option.textContent = suggestion.completionLabel || suggestion.fieldLabel;
            if (suggestion.completionDescription) {
                var description = document.createElement('small');
                description.textContent = suggestion.completionDescription;
                description.classList.add('token-autocomplete-suggestion-description');
                option.appendChild(description);
            }
            return option;
        },
        _b);
    return TokenAutocomplete;
}());
//# sourceMappingURL=token-autocomplete.js.map