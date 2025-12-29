var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
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
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __setFunctionName = (this && this.__setFunctionName) || function (f, name, prefix) {
    if (typeof name === "symbol") name = name.description ? "[".concat(name.description, "]") : "";
    return Object.defineProperty(f, "name", { configurable: true, value: prefix ? "".concat(prefix, " ", name) : name });
};
var __asyncValues = (this && this.__asyncValues) || function (o) {
    if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
    var m = o[Symbol.asyncIterator], i;
    return m ? m.call(o) : (o = typeof __values === "function" ? __values(o) : o[Symbol.iterator](), i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i);
    function verb(n) { i[n] = o[n] && function (v) { return new Promise(function (resolve, reject) { v = o[n](v), settle(resolve, reject, v.done, v.value); }); }; }
    function settle(resolve, reject, d, v) { Promise.resolve(v).then(function(v) { resolve({ value: v, done: d }); }, reject); }
};
var SelectModes;
(function (SelectModes) {
    SelectModes[SelectModes["SINGLE"] = 0] = "SINGLE";
    SelectModes[SelectModes["MULTI"] = 1] = "MULTI";
    SelectModes[SelectModes["SEARCH"] = 2] = "SEARCH";
})(SelectModes || (SelectModes = {}));
var TokenAutocomplete = /** @class */ (function () {
    function TokenAutocomplete(options) {
        var _this = this;
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
            noMatchesCustomEntriesDescription: null,
            placeholderText: 'enter some text',
            initialTokens: null,
            initialSuggestions: null,
            tokenRenderer: TokenAutocomplete.MultiSelect.defaultRenderer,
            selectMode: SelectModes.MULTI,
            resolveUri: '',
            resolveUriBuilder: function (value) {
                // We have to do this manually instead of using URL, as we can't be sure that a polyfill for IE11 is present
                var querySeparator = _this.options.resolveUri.indexOf('?') >= 0 ? '&' : '?';
                return _this.options.resolveUri + querySeparator + 'value=' + encodeURIComponent(value);
            },
            suggestionsUri: '',
            suggestionsUriBuilder: function (query) {
                // We have to do this manually instead of using URL, as we can't be sure that a polyfill for IE11 is present
                var querySeparator = _this.options.suggestionsUri.indexOf('?') >= 0 ? '&' : '?';
                return _this.options.suggestionsUri + querySeparator + 'query=' + encodeURIComponent(query);
            },
            suggestionRenderer: TokenAutocomplete.Autocomplete.defaultRenderer,
            minCharactersForSuggestion: 1,
            allowCustomEntries: true,
            readonly: false,
            optional: false,
            showClearButton: false,
            enableTabulator: true,
            showSuggestionsOnFocus: true,
            requestDelay: 200
        };
        this.options = __assign(__assign({}, this.defaults), options);
        if (this.options.selector instanceof HTMLElement) {
            this.container = this.options.selector;
        }
        else {
            var passedContainer = document.querySelector(this.options.selector);
            if (!passedContainer) {
                throw new Error('passed selector does not point to a DOM element.');
            }
            this.container = passedContainer;
        }
        this.container.classList.add('token-autocomplete-container');
        this.tokenContainer = document.createElement('div');
        this.tokenContainer.classList.add('token-autocomplete-tokens');
        this.container.appendChild(this.tokenContainer);
        if (!Array.isArray(this.options.initialTokens) && !Array.isArray(this.options.initialSuggestions)) {
            this.parseTokensAndSuggestions();
        }
        this.hiddenSelect = document.createElement('select');
        this.hiddenSelect.id = this.container.id + '-select';
        this.hiddenSelect.name = this.options.name;
        this.hiddenSelect.ariaHidden = 'true';
        this.hiddenSelect.setAttribute('multiple', 'true');
        this.hiddenSelect.setAttribute('autocomplete', 'off');
        this.hiddenSelect.style.display = 'none';
        // If the field is readonly, we don't want to show the clear button.
        if (this.options.readonly) {
            this.options.showClearButton = false;
            if (this.options.tokenRenderer === TokenAutocomplete.MultiSelect.defaultRenderer) {
                this.options.tokenRenderer = TokenAutocomplete.MultiSelect.defaultReadonlyRenderer;
            }
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
                var _c, _d, _e;
                event.preventDefault();
                if (event.clipboardData) {
                    //  Normal handling for modern browsers
                    var text = (_c = event.clipboardData) === null || _c === void 0 ? void 0 : _c.getData("text/plain");
                    document.execCommand("insertHTML", false, text);
                }
                else {
                    // Fallback logic for IE11
                    var globalText = (_d = window.clipboardData) === null || _d === void 0 ? void 0 : _d.getData("Text");
                    var range = (_e = document.getSelection()) === null || _e === void 0 ? void 0 : _e.getRangeAt(0);
                    range === null || range === void 0 ? void 0 : range.insertNode(document.createTextNode(globalText));
                }
            });
        }
        else {
            this.container.classList.add('token-autocomplete-readonly');
        }
        this.tokenContainer.appendChild(this.textInput);
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
            this.enrichInitialTokens().then(function (shouldUpdate) {
                if (shouldUpdate) {
                    _this.val(_this.options.initialTokens);
                }
            });
        }
        this.container.tokenAutocomplete = this;
        if (this.options.selectMode == SelectModes.SINGLE && !this.options.optional && this.val().length == 0) {
            this.autocomplete.loadSuggestions();
        }
    }
    TokenAutocomplete.prototype.enrichInitialTokens = function () {
        return __awaiter(this, void 0, void 0, function () {
            var _loop_1, this_1, _c, _d, _e, e_1_1;
            var _f, e_1, _g, _h;
            return __generator(this, function (_j) {
                switch (_j.label) {
                    case 0:
                        if (!(this.options.resolveUri.length > 0 && this.options.initialTokens)) return [3 /*break*/, 14];
                        _j.label = 1;
                    case 1:
                        _j.trys.push([1, 7, 8, 13]);
                        _loop_1 = function () {
                            var token, resolveUri;
                            return __generator(this, function (_k) {
                                switch (_k.label) {
                                    case 0:
                                        _h = _e.value;
                                        _c = false;
                                        token = _h;
                                        resolveUri = this_1.options.resolveUriBuilder(token.value);
                                        return [4 /*yield*/, fetch(resolveUri).then(function (response) { return response.json(); }).then(function (data) {
                                                var _c, _d, _e;
                                                token.text = (_c = data.text) !== null && _c !== void 0 ? _c : token.text;
                                                token.value = (_d = data.value) !== null && _d !== void 0 ? _d : token.value;
                                                token.type = (_e = data.type) !== null && _e !== void 0 ? _e : token.type;
                                            })];
                                    case 1:
                                        _k.sent();
                                        return [2 /*return*/];
                                }
                            });
                        };
                        this_1 = this;
                        _c = true, _d = __asyncValues(this.options.initialTokens);
                        _j.label = 2;
                    case 2: return [4 /*yield*/, _d.next()];
                    case 3:
                        if (!(_e = _j.sent(), _f = _e.done, !_f)) return [3 /*break*/, 6];
                        return [5 /*yield**/, _loop_1()];
                    case 4:
                        _j.sent();
                        _j.label = 5;
                    case 5:
                        _c = true;
                        return [3 /*break*/, 2];
                    case 6: return [3 /*break*/, 13];
                    case 7:
                        e_1_1 = _j.sent();
                        e_1 = { error: e_1_1 };
                        return [3 /*break*/, 13];
                    case 8:
                        _j.trys.push([8, , 11, 12]);
                        if (!(!_c && !_f && (_g = _d.return))) return [3 /*break*/, 10];
                        return [4 /*yield*/, _g.call(_d)];
                    case 9:
                        _j.sent();
                        _j.label = 10;
                    case 10: return [3 /*break*/, 12];
                    case 11:
                        if (e_1) throw e_1.error;
                        return [7 /*endfinally*/];
                    case 12: return [7 /*endfinally*/];
                    case 13: return [2 /*return*/, true];
                    case 14: return [2 /*return*/, false];
                }
            });
        });
    };
    /**
     * Searches the element given as a container for option elements and creates active tokens (when the option is marked selected)
     * and suggestions (all options found) from these. During this all found options are removed from the DOM.
     */
    TokenAutocomplete.prototype.parseTokensAndSuggestions = function () {
        var _this = this;
        var initialTokens = [];
        var initialSuggestions = [];
        var options = this.container.querySelectorAll('option');
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
                    completionDescription: option.dataset.description || null,
                    completionLabel: null,
                    disabled: option.disabled || false
                });
            }
            _this.container.removeChild(option);
        });
        if (initialSuggestions.length > 0) {
            this.options.initialSuggestions = initialSuggestions;
            if (!this.options.optional && initialTokens.length == 0) {
                var firstSuggestion = initialSuggestions[0];
                initialTokens.push({
                    value: firstSuggestion.value, text: firstSuggestion.fieldLabel, type: firstSuggestion.type
                });
            }
        }
        if (initialTokens.length > 0) {
            this.options.initialTokens = initialTokens;
        }
    };
    /**
     * Clears the currently present tokens and creates new ones from the given input value, returns new tokens afterwards.
     *
     * The current tokens are only overwritten (cleared and added) when a value parameter is given.
     * In addition to the possibility of setting the value of the input this method also returns the values of all present tokens.
     *
     * @param {(Array<Token>|Token)} value - either the name of a single token or a list of tokens to create
     * @param {boolean} silent - whether appropriate events should be triggered when changing tokens or not
     *
     * @returns an array of the values of all current (after update) tokens of the input field
     */
    TokenAutocomplete.prototype.val = function (value, silent) {
        if (value === void 0) { value = null; }
        if (silent === void 0) { silent = false; }
        if (typeof value !== 'undefined' && value !== null) {
            this.select.clear(silent);
            this.addToken(value, silent);
        }
        var tokens = [];
        this.hiddenSelect.querySelectorAll('option').forEach(function (option) {
            if (option.dataset.value != null && option.dataset.value !== "") {
                tokens.push(option.dataset.value);
            }
        });
        return tokens;
    };
    /**
     * Adds the given tokens to the field.
     *
     * The current tokens are only added when a value parameter is given.
     *
     * @param {(Array<Token>|Token)} value - either the name of a single token or a list of tokens to create
     * @param {boolean} silent - whether appropriate events should be triggered when changing tokens or not
     */
    TokenAutocomplete.prototype.addToken = function (value, silent) {
        var _this = this;
        if (silent === void 0) { silent = false; }
        if (Array.isArray(value)) {
            value.forEach(function (token) {
                if (typeof token === 'object') {
                    _this.select.addToken(token.value, token.text, token.type, silent);
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
        this.select.updateHasValue();
        if (silent) {
            return;
        }
        this.container.dispatchEvent(new CustomEvent('query-changed', {
            detail: {
                query: input
            }
        }));
    };
    TokenAutocomplete.prototype.addHiddenOption = function (tokenValue, tokenText, tokenType, isLiveEntry) {
        if (isLiveEntry === void 0) { isLiveEntry = false; }
        tokenValue = tokenValue.trim();
        tokenText = tokenText.trim();
        var _emptyToken = this.hiddenSelect.querySelector('.empty-token');
        if (_emptyToken) {
            this.hiddenSelect.removeChild(_emptyToken);
        }
        var _existingLiveEntry = this.hiddenSelect.querySelector('.token-autocomplete-live-entry');
        if (_existingLiveEntry) {
            this.hiddenSelect.removeChild(_existingLiveEntry);
        }
        var _existingOption = this.findOptionWithValue(tokenValue);
        if (_existingOption) {
            this.hiddenSelect.removeChild(_existingOption);
        }
        var option = document.createElement('option');
        option.text = tokenText;
        option.value = tokenValue;
        option.selected = true;
        option.dataset.text = tokenText;
        option.dataset.value = tokenValue;
        if (tokenType != null) {
            option.dataset.type = tokenType;
        }
        if (isLiveEntry) {
            option.classList.add('token-autocomplete-live-entry');
        }
        this.hiddenSelect.add(option);
    };
    TokenAutocomplete.prototype.findOptionWithValue = function (optionValue) {
        for (var i = 0; i < this.hiddenSelect.options.length; i++) {
            var option = this.hiddenSelect.options[i];
            if (option.value === optionValue) {
                return option;
            }
        }
        return null;
    };
    TokenAutocomplete.prototype.addHiddenEmptyOption = function () {
        var _emptyToken = this.hiddenSelect.querySelector('.empty-token');
        if (_emptyToken) {
            _emptyToken.setAttribute('selected', 'true');
        }
        else {
            var _newOption = document.createElement('option');
            _newOption.text = '';
            _newOption.value = '';
            _newOption.selected = true;
            _newOption.classList.add('empty-token');
            this.hiddenSelect.add(_newOption);
        }
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
                // Intentionally left empty to only log when debugging is enabled.
            };
        }
    };
    TokenAutocomplete.escapeQuotes = function (text) {
        var _c;
        return (_c = text === null || text === void 0 ? void 0 : text.replace(/\x22/g, '\\\x22')) !== null && _c !== void 0 ? _c : '';
    };
    TokenAutocomplete.shouldIgnoreSuggestion = function (suggestion) {
        if (suggestion instanceof HTMLElement) {
            return suggestion.dataset.disabled === 'true';
        }
        return true;
    };
    var _a, _b;
    TokenAutocomplete.MultiSelect = (_a = /** @class */ (function () {
            function class_1(parent) {
                this.parent = parent;
                this.container = parent.container;
                this.tokenContainer = parent.tokenContainer;
                this.options = parent.options;
                this.renderer = parent.options.tokenRenderer;
                if (this.options.showClearButton) {
                    var clearButton = document.createElement('span');
                    clearButton.classList.add('token-autocomplete-delete-button');
                    this.container.appendChild(clearButton);
                }
            }
            class_1.prototype.clearCurrentInput = function (ignored) {
                if (ignored === void 0) { ignored = false; }
                var previousInput = this.parent.getCurrentInput();
                this.parent.textInput.textContent = '';
                this.parent.log('cleared input', previousInput);
            };
            class_1.prototype.initEventListeners = function () {
                var _this = this;
                var _c;
                var parent = this.parent;
                if (parent.options.readonly) {
                    return;
                }
                var isComposing = false;
                parent.textInput.addEventListener('compositionstart', function () {
                    isComposing = true;
                });
                parent.textInput.addEventListener('compositionend', function (event) {
                    isComposing = false;
                    // handles hitting ENTER on GBoard, which uses composition events instead of individual key triggers
                    var inputString = event.data;
                    if (inputString.charAt(inputString.length - 1) === "\n") {
                        event.preventDefault();
                        _this.handleInput(parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted'));
                    }
                });
                parent.textInput.addEventListener('keydown', function (event) {
                    if (isComposing)
                        return;
                    if (event.key == parent.KEY_ENTER || (event.key == parent.KEY_TAB && parent.options.enableTabulator && parent.autocomplete.areSuggestionsDisplayed() && parent.autocomplete.suggestions.childNodes.length == 1)) {
                        event.preventDefault();
                        var highlightedSuggestion = parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                        if (parent.options.enableTabulator && highlightedSuggestion == null && event.key == parent.KEY_TAB && parent.autocomplete.areSuggestionsDisplayed()) {
                            highlightedSuggestion = parent.autocomplete.suggestions.firstChild;
                        }
                        _this.handleInput(highlightedSuggestion);
                    }
                    else if (parent.getCurrentInput() === '' && event.key == parent.KEY_BACKSPACE) {
                        event.preventDefault();
                        _this.removeLastToken();
                    }
                    if ((event.key == parent.KEY_DOWN || event.key == parent.KEY_UP) && parent.autocomplete.suggestions.childNodes.length > 0) {
                        event.preventDefault();
                    }
                });
                parent.textInput.addEventListener('keyup', function () { return _this.updateHasValue(); });
                if (!(this instanceof TokenAutocomplete.SearchMultiSelect)) {
                    // We don't want to trigger a search when the user focuses out of the input field in a search field.
                    // This should only try to apply the current input text as value in normal multi-selects.
                    parent.textInput.addEventListener('focusout', function (event) {
                        // Using setTimeout here seems hacky on first sight but ensures proper order of events / handling.
                        // We first want to handle a click on a suggestion (when one is made) before hiding the suggestions on focusout of the input.
                        // Not doing so could mean the suggestion is hidden before the click is handled und thus resulting in not being selected.
                        // This depends on the order in which a browser handles different events and when it sets the active pseudo-selector on clicked events (Firefox for example)
                        setTimeout(function () {
                            if (parent.autocomplete.areSuggestionsActive()) {
                                return;
                            }
                            var input = _this.parent.getCurrentInput();
                            if (input != '' && !_this.handleInputAsValue(input)) {
                                _this.container.dispatchEvent(new CustomEvent('input-ignored', {
                                    detail: {
                                        input: input
                                    }
                                }));
                            }
                        }, 0);
                    });
                }
                if (this.options.showClearButton) {
                    (_c = parent.container.querySelector('.token-autocomplete-delete-button')) === null || _c === void 0 ? void 0 : _c.addEventListener('click', function () {
                        _this.clear(true);
                        _this.clearCurrentInput();
                        _this.updateHasValue();
                        _this.container.dispatchEvent(new CustomEvent('input-cleared'));
                    });
                }
            };
            class_1.prototype.handleInput = function (highlightedSuggestion) {
                if (highlightedSuggestion !== null) {
                    this.clearCurrentInput();
                    if (highlightedSuggestion.classList.contains('token-autocomplete-suggestion-active')) {
                        this.removeTokenWithText(highlightedSuggestion.dataset.tokenText);
                    }
                    else {
                        if (highlightedSuggestion.dataset.becomesToken !== 'false') {
                            this.addToken(highlightedSuggestion.dataset.value, highlightedSuggestion.dataset.tokenText, highlightedSuggestion.dataset.type, false);
                        }
                        this.parent.autocomplete.dispatchSuggestionSelectedEvent(highlightedSuggestion);
                    }
                }
                else {
                    this.handleInputAsValue(this.parent.getCurrentInput());
                }
                this.parent.autocomplete.clearSuggestions();
                this.parent.autocomplete.hideSuggestions();
            };
            /**
             * Updates the 'token-autocomplete-has-value' class of this MultiSelect autocomplete.
             */
            class_1.prototype.updateHasValue = function () {
                if (this.parent.getCurrentInput() === '' && this.parent.val().length === 0) {
                    this.container.classList.remove('token-autocomplete-has-value');
                }
                else {
                    this.container.classList.add('token-autocomplete-has-value');
                }
            };
            /**
             * Adds the current user input as a net token and resets the input area so new text can be entered.
             *
             * @param {string} input - the actual input the user entered
             * @returns {boolean} - whether the input was handled as a value (true) or discarded (false)
             */
            class_1.prototype.handleInputAsValue = function (input) {
                if (input != '' && this.parent.options.allowCustomEntries) {
                    this.clearCurrentInput();
                    this.addToken(input, input, null);
                    return true;
                }
                if (this.parent.autocomplete.suggestions.childNodes.length === 1 && this.parent.autocomplete.suggestions.childNodes[0].dataset.value != '_no_match_') {
                    this.parent.autocomplete.suggestions.firstChild.click();
                    return true;
                }
                else {
                    this.clearCurrentInput();
                    return false;
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
                var _this = this;
                var _c;
                if (silent === void 0) { silent = false; }
                if (tokenValue === null || tokenText === null || tokenValue === '_no_match_') {
                    return;
                }
                tokenValue = tokenValue.trim();
                tokenText = tokenText.trim();
                this.parent.addHiddenOption(tokenValue, tokenText, tokenType);
                var addedToken = {
                    value: tokenValue,
                    text: tokenText,
                    type: tokenType
                };
                var element = this.renderer(addedToken);
                (_c = element.querySelector('.token-autocomplete-delete-button')) === null || _c === void 0 ? void 0 : _c.addEventListener('click', function () {
                    _this.removeToken(element);
                });
                this.tokenContainer.insertBefore(element, this.parent.textInput);
                this.updateHasValue();
                if (!silent) {
                    this.container.dispatchEvent(new CustomEvent('tokens-changed', {
                        detail: {
                            tokens: this.parent.val(),
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
                var _this = this;
                if (silent === void 0) { silent = false; }
                var tokens = this.tokenContainer.querySelectorAll('.token-autocomplete-token');
                tokens.forEach(function (token) { return _this.removeToken(token, silent); });
            };
            /**
             * Removes the last token in the list of currently present token. This is the last added token next to the input field.
             */
            class_1.prototype.removeLastToken = function () {
                var tokens = this.tokenContainer.querySelectorAll('.token-autocomplete-token');
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
                var _c;
                if (silent === void 0) { silent = false; }
                this.tokenContainer.removeChild(token);
                var tokenText = token.dataset.text;
                var hiddenOption = this.parent.hiddenSelect.querySelector('option[data-text="' + TokenAutocomplete.escapeQuotes(tokenText) + '"]');
                (_c = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.parentElement) === null || _c === void 0 ? void 0 : _c.removeChild(hiddenOption);
                var addedToken = {
                    value: token.dataset.value,
                    text: tokenText,
                    type: token.dataset.type || null
                };
                if (!silent) {
                    this.container.dispatchEvent(new CustomEvent('tokens-changed', {
                        detail: {
                            tokens: this.parent.val(),
                            removed: addedToken
                        }
                    }));
                }
                if (this.parent.val().length === 0) {
                    this.parent.addHiddenEmptyOption();
                }
                this.updateHasValue();
                this.parent.log('removed token', token.textContent);
            };
            class_1.prototype.removeTokenWithText = function (tokenText) {
                if (tokenText === null) {
                    return;
                }
                var token = this.tokenContainer.querySelector('.token-autocomplete-token[data-text="' + TokenAutocomplete.escapeQuotes(tokenText) + '"]');
                if (token !== null) {
                    this.removeToken(token);
                }
            };
            return class_1;
        }()),
        __setFunctionName(_a, "MultiSelect"),
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
            deleteToken.classList.add('token-autocomplete-delete-button');
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
                var clearButton = document.createElement('span');
                clearButton.classList.add('token-autocomplete-delete-button');
                this.container.appendChild(clearButton);
            }
            this.toggleButton = document.createElement('button');
            this.toggleButton.classList.add('token-autocomplete-toggle-button');
            this.toggleButton.type = 'button';
            this.toggleButton.tabIndex = 0;
            this.container.appendChild(this.toggleButton);
        }
        /**
         * Clears the current user input so new text can be entered.
         *
         * @param {boolean} silent - whether an appropriate event should be triggered
         * @param {boolean} keepPreviousValue - if true, the previous value will be stored and shown as a placeholder
         */
        class_2.prototype.clear = function (silent, keepPreviousValue) {
            var _c;
            if (keepPreviousValue === void 0) { keepPreviousValue = true; }
            if (this.options.readonly) {
                return;
            }
            var tokenText = this.parent.textInput.textContent;
            var hiddenOption = this.parent.hiddenSelect.querySelector('option[data-text="' + TokenAutocomplete.escapeQuotes(tokenText) + '"]');
            this.container.classList.remove('token-autocomplete-has-value');
            var previousValue = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.dataset.value;
            var previousText = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.dataset.text;
            var previousType = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.dataset.type;
            if (keepPreviousValue) {
                this.previousValue = previousValue;
                this.previousText = previousText;
                this.previousType = previousType;
                if (hiddenOption == null && this.options.allowCustomEntries) {
                    this.previousValue = tokenText;
                    this.previousText = tokenText;
                }
                if (this.previousText && this.previousText !== '') {
                    this.parent.textInput.dataset.placeholder = this.previousText;
                }
            }
            else {
                // We should reset these fields, so they are not used to restore the previously selected value
                // when the focusout event is handled after the click event on the suggestion.
                delete this.previousValue;
                delete this.previousText;
                delete this.previousType;
                if (this.parent.options.placeholderText != null) {
                    this.parent.textInput.dataset.placeholder = this.parent.options.placeholderText;
                }
            }
            (_c = hiddenOption === null || hiddenOption === void 0 ? void 0 : hiddenOption.parentElement) === null || _c === void 0 ? void 0 : _c.removeChild(hiddenOption);
            this.parent.addHiddenEmptyOption();
            this.parent.textInput.textContent = '';
            this.parent.textInput.contentEditable = 'true';
            if (!silent) {
                this.container.dispatchEvent(new CustomEvent('tokens-changed', {
                    detail: {
                        tokens: this.parent.val(),
                        removed: {
                            value: previousValue,
                            text: previousText,
                            type: previousType
                        }
                    }
                }));
            }
        };
        /**
         * Adds the current user input as a net token and resets the input area so new text can be entered.
         *
         * @param {string} input - the actual input the user entered
         * @returns {boolean} - whether the input was handled as a value (true) or discarded (false)
         */
        class_2.prototype.handleInputAsValue = function (input) {
            if (input != '' && this.parent.options.allowCustomEntries) {
                this.clearCurrentInput();
                this.addToken(input, input, null, false);
                this.parent.autocomplete.clearSuggestions();
                this.parent.autocomplete.hideSuggestions();
                return true;
            }
            if (this.parent.autocomplete.suggestions.childNodes.length === 1 && this.parent.autocomplete.suggestions.childNodes[0].dataset.value != '_no_match_') {
                this.parent.autocomplete.suggestions.firstChild.click();
                return true;
            }
            this.clear(true, false);
            return false;
        };
        class_2.prototype.clearCurrentInput = function () {
            this.clear(true);
        };
        /**
         * Updates the 'token-autocomplete-has-value' class of this SingleSelect autocomplete.
         */
        class_2.prototype.updateHasValue = function () {
            if (this.parent.getCurrentInput() === '' && this.parent.val().length === 0) {
                this.container.classList.remove('token-autocomplete-has-value');
            }
            else {
                this.container.classList.add('token-autocomplete-has-value');
            }
        };
        class_2.prototype.addToken = function (tokenValue, tokenText, tokenType, silent) {
            if (tokenValue === null || tokenText === null || tokenValue === '_no_match_') {
                return;
            }
            tokenValue = tokenValue.trim();
            tokenText = tokenText.trim();
            this.clear(true, false);
            this.parent.textInput.textContent = tokenText;
            this.parent.textInput.contentEditable = 'false';
            this.parent.textInput.blur();
            if (tokenText !== '') {
                this.container.classList.add('token-autocomplete-has-value');
            }
            this.parent.addHiddenOption(tokenValue, tokenText, tokenType);
            if (!silent) {
                this.container.dispatchEvent(new CustomEvent('tokens-changed', {
                    detail: {
                        tokens: this.parent.val(),
                        added: {
                            value: tokenValue,
                            text: tokenText,
                            type: tokenType
                        }
                    }
                }));
            }
        };
        class_2.prototype.initEventListeners = function () {
            var _this = this;
            var _c;
            var parent = this.parent;
            if (parent.options.readonly) {
                return;
            }
            parent.textInput.addEventListener('compositionend', function (event) {
                // handles hitting ENTER on GBoard, which uses composition events instead of individual key triggers
                var inputString = event.data;
                if (inputString.charAt(inputString.length - 1) === "\n") {
                    event.preventDefault();
                    _this.handleInput(parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted'));
                }
            });
            parent.textInput.addEventListener('keydown', function (event) {
                if (event.key == parent.KEY_ENTER || (event.key == parent.KEY_TAB && parent.options.enableTabulator && parent.autocomplete.areSuggestionsDisplayed() && parent.autocomplete.suggestions.childNodes.length == 1)) {
                    event.preventDefault();
                    var highlightedSuggestion = parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                    if (parent.options.enableTabulator && highlightedSuggestion == null && event.key == parent.KEY_TAB && parent.autocomplete.areSuggestionsDisplayed()) {
                        highlightedSuggestion = parent.autocomplete.suggestions.firstChild;
                    }
                    _this.handleInput(highlightedSuggestion);
                }
                if ((event.key == parent.KEY_DOWN || event.key == parent.KEY_UP) && parent.autocomplete.suggestions.childNodes.length > 0) {
                    event.preventDefault();
                }
            });
            var focusInput = function () {
                if (!parent.autocomplete.areSuggestionsDisplayed() && parent.options.showSuggestionsOnFocus) {
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
            };
            parent.textInput.addEventListener('click', function () { return focusInput(); });
            parent.textInput.addEventListener('focusout', function (event) {
                if (event.relatedTarget === _this.toggleButton && parent.autocomplete.areSuggestionsDisplayed()) {
                    // If the focus is moved to the toggle button, we mark it so the click handler does not set focus again.
                    _this.toggleButton.dataset.inputWasFocused = 'true';
                }
                // Using setTimeout here seems hacky on first sight but ensures proper order of events / handling.
                // We first want to handle a click on a suggestion (when one is made) before hiding the suggestions on focusout of the input.
                // Not doing so could mean the suggestion is hidden before the click is handled und thus resulting in not being selected.
                // This depends on the order in which a browser handles different events and when it sets the active pseudo-selector on clicked events (Firefox for example)
                setTimeout(function () {
                    if (parent.autocomplete.areSuggestionsActive()) {
                        return;
                    }
                    var input = _this.parent.getCurrentInput();
                    if (_this.parent.val().length !== 0 && _this.parent.val()[0] !== '') {
                        return;
                    }
                    if (input != '') {
                        if (!_this.handleInputAsValue(input)) {
                            _this.container.dispatchEvent(new CustomEvent('input-ignored', {
                                detail: {
                                    input: input
                                }
                            }));
                        }
                        return;
                    }
                    if (_this.previousValue) {
                        _this.addToken(_this.previousValue, _this.previousText, _this.previousType, true);
                    }
                }, 0);
            });
            (_c = parent.container.querySelector('.token-autocomplete-delete-button')) === null || _c === void 0 ? void 0 : _c.addEventListener('click', function () {
                _this.clear(false, false);
            });
            this.toggleButton.addEventListener('click', function () {
                if (_this.toggleButton.dataset.inputWasFocused === 'true') {
                    // The focus was moved to the toggle button, so we do not want to focus the input again.
                    delete _this.toggleButton.dataset.inputWasFocused;
                    _this.toggleButton.blur();
                }
                else {
                    // If the input is not focused, we want to focus it and show the suggestions.
                    focusInput();
                }
            });
        };
        class_2.prototype.handleInput = function (highlightedSuggestion) {
            if (highlightedSuggestion !== null) {
                this.addToken(highlightedSuggestion.dataset.value, highlightedSuggestion.dataset.tokenText, highlightedSuggestion.dataset.type, false);
            }
            else {
                this.handleInputAsValue(this.parent.getCurrentInput());
            }
            this.parent.autocomplete.clearSuggestions();
            this.parent.autocomplete.hideSuggestions();
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
         * @returns {boolean} - whether the input was handled as a value (true) or discarded (false)
         */
        class_3.prototype.handleInputAsValue = function (input) {
            this.container.dispatchEvent(new CustomEvent('query-changed', {
                detail: {
                    query: input
                }
            }));
            return true;
        };
        return class_3;
    }(TokenAutocomplete.MultiSelect));
    TokenAutocomplete.Autocomplete = (_b = /** @class */ (function () {
            function class_4(parent) {
                var _this = this;
                this.scrollParent = null;
                this.hideHandler = function () {
                    _this.hideSuggestions();
                };
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
                var _this = this;
                if (this.parent.options.readonly) {
                    return;
                }
                this.parent.textInput.addEventListener('keyup', function (event) {
                    if (event.key == _this.parent.KEY_ESC) {
                        _this.hideSuggestions();
                        _this.parent.textInput.blur();
                        return;
                    }
                    if (event.key == _this.parent.KEY_UP && _this.suggestions.childNodes.length > 0) {
                        event.preventDefault();
                        var highlightedSuggestion = _this.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                        if (highlightedSuggestion == null) {
                            // highlight last entry and scroll to bottom
                            var bottomSuggestion = _this.suggestions.lastChild;
                            while (bottomSuggestion != null && TokenAutocomplete.shouldIgnoreSuggestion(bottomSuggestion)) {
                                bottomSuggestion = bottomSuggestion.previousSibling;
                            }
                            if (bottomSuggestion != null) {
                                _this.highlightSuggestion(bottomSuggestion);
                                _this.suggestions.scrollTop = _this.suggestions.scrollHeight;
                            }
                            return;
                        }
                        var aboveSuggestion = highlightedSuggestion.previousSibling;
                        while (aboveSuggestion != null && TokenAutocomplete.shouldIgnoreSuggestion(aboveSuggestion)) {
                            aboveSuggestion = aboveSuggestion.previousSibling;
                        }
                        if (aboveSuggestion != null) {
                            // if the suggestions is above the scroll position, scroll to the suggestion
                            var suggestionTop = aboveSuggestion.offsetTop;
                            if (_this.suggestions.scrollTop > suggestionTop) {
                                _this.suggestions.scrollTop = suggestionTop;
                            }
                            _this.highlightSuggestion(aboveSuggestion);
                        }
                        else {
                            highlightedSuggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                        }
                        return;
                    }
                    if (event.key == _this.parent.KEY_DOWN && _this.suggestions.childNodes.length > 0) {
                        event.preventDefault();
                        var highlightedSuggestion = _this.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                        if (highlightedSuggestion == null) {
                            // highlight first entry and scroll to top
                            var topSuggestion = _this.suggestions.firstChild;
                            while (topSuggestion != null && TokenAutocomplete.shouldIgnoreSuggestion(topSuggestion)) {
                                topSuggestion = topSuggestion.nextSibling;
                            }
                            if (topSuggestion != null) {
                                _this.highlightSuggestion(topSuggestion);
                                _this.suggestions.scrollTop = 0;
                            }
                            return;
                        }
                        var belowSuggestion = highlightedSuggestion === null || highlightedSuggestion === void 0 ? void 0 : highlightedSuggestion.nextSibling;
                        while (belowSuggestion != null && TokenAutocomplete.shouldIgnoreSuggestion(belowSuggestion)) {
                            belowSuggestion = belowSuggestion.nextSibling;
                        }
                        if (belowSuggestion != null) {
                            // if the suggestions is not completely visible, scroll until the suggestion is at the bottom
                            var suggestionBottom = belowSuggestion.offsetTop + belowSuggestion.offsetHeight;
                            if (_this.suggestions.scrollTop + _this.suggestions.clientHeight < suggestionBottom) {
                                _this.suggestions.scrollTop = suggestionBottom - _this.suggestions.clientHeight;
                            }
                            _this.highlightSuggestion(belowSuggestion);
                        }
                        else {
                            highlightedSuggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                        }
                        return;
                    }
                    if (event.key == _this.parent.KEY_LEFT || event.key == _this.parent.KEY_RIGHT || event.key == _this.parent.KEY_ENTER || event.key == _this.parent.KEY_TAB) {
                        // We don't want to re-trigger the autocompletion when the user navigates the cursor inside the input.
                        return;
                    }
                    _this.loadSuggestions();
                });
                this.parent.textInput.addEventListener('focusout', function () {
                    // Using setTimeout here seems hacky on first sight but ensures proper order of events / handling.
                    // We first want to handle a click on a suggestion (when one is made) before hiding the suggestions on focusout of the input.
                    // Not doing so could mean the suggestion is hidden before the click is handled und thus resulting in not being selected.
                    // This depends on the order in which a browser handles different events and when it sets the active pseudo-selector on clicked events (Firefox for example)
                    setTimeout(function () {
                        if (_this.areSuggestionsActive()) {
                            return;
                        }
                        _this.hideSuggestions();
                    }, 0);
                });
                this.parent.textInput.addEventListener('focusin', function () {
                    if (_this.options.showSuggestionsOnFocus) {
                        _this.loadSuggestions();
                        _this.showSuggestions();
                    }
                });
            };
            class_4.prototype.loadSuggestions = function () {
                var _this = this;
                var value = this.parent.getCurrentInput();
                if (this.parent.options.selectMode == SelectModes.SINGLE) {
                    if (!this.parent.textInput.isContentEditable) {
                        this.parent.select.clear(true);
                        value = "";
                    }
                }
                else if (value.length < this.parent.options.minCharactersForSuggestion) {
                    this.clearSuggestions();
                    this.hideSuggestions();
                    return;
                }
                if (this.parent.options.suggestionsUri.length > 0) {
                    this.requestSuggestions(value);
                    return;
                }
                if (Array.isArray(this.parent.options.initialSuggestions)) {
                    this.clearSuggestions();
                    this.parent.options.initialSuggestions.forEach(function (suggestion) {
                        if (typeof suggestion !== 'object') {
                            // The suggestion is of wrong type and therefore ignored.
                            return;
                        }
                        var text = suggestion.fieldLabel;
                        if (value.length == 0 && _this.options.selectMode == SelectModes.SINGLE && !_this.options.optional && !_this.areSuggestionsDisplayed()) {
                            _this.addSuggestion(suggestion, false);
                        }
                        else if (value.localeCompare(text.slice(0, value.length), undefined, { sensitivity: 'base' }) === 0) {
                            // The suggestion starts with the query text the user entered and will be displayed.
                            _this.addSuggestion(suggestion);
                        }
                    });
                    if (value.length >= this.parent.options.minCharactersForSuggestion) {
                        var hasExactMatch = this.suggestions.querySelector("li[data-value='".concat(value, "']:not([data-type='_no_match_']),li[data-text='").concat(value, "']:not([data-type='_no_match_'])"));
                        if (!hasExactMatch && this.parent.options.allowCustomEntries && this.parent.options.noMatchesCustomEntriesDescription) {
                            this.addSuggestion({
                                id: null,
                                value: value,
                                fieldLabel: value,
                                type: '_no_match_',
                                completionDescription: this.parent.options.noMatchesCustomEntriesDescription,
                                completionLabel: null,
                                disabled: false
                            });
                        }
                        else if (this.suggestions.childNodes.length == 0 && this.parent.options.noMatchesText) {
                            this.addSuggestion({
                                id: null,
                                value: '_no_match_',
                                fieldLabel: this.parent.options.noMatchesText,
                                type: '_no_match_',
                                completionDescription: null,
                                completionLabel: null,
                                disabled: true
                            });
                        }
                    }
                }
            };
            /**
             * Hides the suggestions dropdown from the user.
             */
            class_4.prototype.hideSuggestions = function () {
                var _c;
                // as the suggestions will be re-shown if a pending request is executed, we abort them if we want to hide
                this.abortPendingRequest();
                this.container.classList.remove('token-autocomplete-suggestions-displayed');
                this.suggestions.style.display = '';
                var _highlightedSuggestions = this.suggestions.querySelectorAll('li.token-autocomplete-suggestion-highlighted');
                _highlightedSuggestions.forEach(function (_suggestion) {
                    _suggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                });
                window.removeEventListener('resize', this.hideHandler);
                (_c = this.scrollParent) === null || _c === void 0 ? void 0 : _c.removeEventListener('scroll', this.hideHandler);
            };
            /**
             * Shows the suggestions dropdown to the user.
             */
            class_4.prototype.showSuggestions = function () {
                var _c;
                this.container.classList.add('token-autocomplete-suggestions-displayed');
                this.suggestions.style.display = 'block';
                this.updateSuggestionsPosition();
                window.addEventListener('resize', this.hideHandler);
                this.scrollParent = this.findScrollParent(this.container);
                (_c = this.scrollParent) === null || _c === void 0 ? void 0 : _c.addEventListener('scroll', this.hideHandler);
            };
            class_4.prototype.updateSuggestionsPosition = function () {
                if (!this.areSuggestionsDisplayed()) {
                    return;
                }
                var containerBox = this.container.getBoundingClientRect();
                var suggestionsHeight = this.suggestions.offsetHeight;
                var viewportHeight = window.innerHeight;
                var spaceBelow = viewportHeight - containerBox.bottom;
                this.suggestions.style.left = "".concat(containerBox.left, "px");
                this.suggestions.style.width = "".concat(containerBox.width, "px");
                if (spaceBelow < suggestionsHeight) {
                    this.suggestions.style.bottom = "".concat(viewportHeight - containerBox.top, "px");
                    this.suggestions.style.top = 'initial';
                }
                else {
                    this.suggestions.style.top = "".concat(containerBox.bottom, "px");
                    this.suggestions.style.bottom = 'initial';
                }
            };
            class_4.prototype.findScrollParent = function (_scrollingElement) {
                if (_scrollingElement == null) {
                    return window;
                }
                if (_scrollingElement === document.documentElement || _scrollingElement === document.body) {
                    return window;
                }
                var overflowY = window.getComputedStyle(_scrollingElement).overflowY;
                var isScrollable = overflowY !== 'visible' && overflowY !== 'hidden';
                if (isScrollable && _scrollingElement.scrollHeight >= _scrollingElement.clientHeight) {
                    return _scrollingElement;
                }
                return this.findScrollParent(_scrollingElement.parentNode);
            };
            class_4.prototype.areSuggestionsDisplayed = function () {
                return this.suggestions.style.display === 'block';
            };
            class_4.prototype.highlightSuggestionAtPosition = function (index) {
                var _suggestions = this.suggestions.querySelectorAll('li');
                _suggestions.forEach(function (_suggestion) {
                    _suggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                });
                _suggestions[index].classList.add('token-autocomplete-suggestion-highlighted');
            };
            class_4.prototype.highlightSuggestion = function (_suggestion) {
                this.suggestions.querySelectorAll('li.token-autocomplete-suggestion-highlighted').forEach(function (_highlightedSuggestion) {
                    _highlightedSuggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                });
                _suggestion.classList.add('token-autocomplete-suggestion-highlighted');
            };
            /**
             * Checks for the presence of a suggestion that is currently clicked via mouse (active).
             */
            class_4.prototype.areSuggestionsActive = function () {
                return !!this.suggestions.querySelector('li:active');
            };
            /**
             * Removes all previous suggestions from the dropdown.
             */
            class_4.prototype.clearSuggestions = function () {
                this.abortPendingRequest();
                this.suggestions.innerHTML = '';
            };
            /**
             * Aborts currently in progress or scheduled suggestions requests.
             */
            class_4.prototype.abortPendingRequest = function () {
                var _c;
                (_c = this.request) === null || _c === void 0 ? void 0 : _c.abort();
                clearTimeout(this.timeout);
            };
            /**
             * Loads suggestions matching the given query from the rest service behind the URI given as an option while initializing the field.
             *
             * @param query the query to search suggestions for
             */
            class_4.prototype.requestSuggestions = function (query) {
                var _this = this;
                clearTimeout(this.timeout);
                if (!this.timeout) {
                    this.debouncedRequestSuggestions.call(this, query);
                    this.timeout = window.setTimeout(function () {
                        delete _this.timeout;
                    }, this.parent.options.requestDelay);
                }
                else {
                    this.timeout = window.setTimeout(function () {
                        delete _this.timeout;
                        _this.debouncedRequestSuggestions.call(_this, query);
                    }, this.parent.options.requestDelay);
                }
            };
            class_4.prototype.dispatchSuggestionSelectedEvent = function (_suggestion) {
                _suggestion.dispatchEvent(new CustomEvent('suggestion-selected', {
                    detail: {
                        value: _suggestion.dataset.value,
                        text: _suggestion.dataset.text,
                        type: _suggestion.dataset.type || null
                    }
                }));
            };
            class_4.prototype.debouncedRequestSuggestions = function (query) {
                var _this = this;
                if (this.request != null && this.request.readyState) {
                    this.request.abort();
                }
                var request = new XMLHttpRequest();
                this.request = request;
                this.request.onload = function () {
                    _this.request = null;
                    _this.clearSuggestions();
                    var answer = request.response;
                    if (answer == null) {
                        return;
                    }
                    // IE 11 doesn't properly respect content type header, need to parse json string by hand.
                    if (typeof answer === 'string') {
                        answer = JSON.parse(answer);
                    }
                    if (Array.isArray(answer.completions)) {
                        if (_this.parent.val().length == 0 && answer.completions.length > 0 && _this.options.selectMode == SelectModes.SINGLE && !_this.options.optional && !_this.areSuggestionsDisplayed()) {
                            answer.completions.forEach(function (suggestion) { return _this.addSuggestion(suggestion, false); });
                            var firstSuggestion = answer.completions[0];
                            var value_1 = firstSuggestion.id || firstSuggestion.value;
                            _this.parent.select.addToken(value_1, firstSuggestion.fieldLabel, firstSuggestion.type, true);
                            return;
                        }
                        answer.completions.forEach(function (suggestion) { return _this.addSuggestion(suggestion); });
                        var value = _this.parent.getCurrentInput();
                        if (value.length >= _this.parent.options.minCharactersForSuggestion) {
                            var hasExactMatch = _this.suggestions.querySelector("li[data-value='".concat(value, "']:not([data-type='_no_match_']),li[data-text='").concat(value, "']:not([data-type='_no_match_'])"));
                            if (!hasExactMatch && _this.parent.options.allowCustomEntries && _this.parent.options.noMatchesCustomEntriesDescription) {
                                _this.addSuggestion({
                                    id: null,
                                    value: query,
                                    fieldLabel: query,
                                    type: '_no_match_',
                                    completionDescription: _this.parent.options.noMatchesCustomEntriesDescription,
                                    completionLabel: null,
                                    disabled: false
                                });
                            }
                            else if (_this.suggestions.childNodes.length == 0 && _this.parent.options.noMatchesText) {
                                _this.addSuggestion({
                                    id: null,
                                    value: '_no_match_',
                                    fieldLabel: _this.parent.options.noMatchesText,
                                    type: '_no_match_',
                                    completionDescription: null,
                                    completionLabel: null,
                                    disabled: true
                                });
                            }
                        }
                    }
                };
                var suggestionsUri = this.options.suggestionsUriBuilder(query);
                this.request.open('GET', suggestionsUri, true);
                this.request.responseType = 'json';
                this.request.setRequestHeader('Content-type', 'application/json');
                this.request.send();
            };
            /**
             * Adds a suggestion with the given text matching the users input to the dropdown.
             *
             * @param {string} suggestion - the metadata of the suggestion that should be added
             * @param showSuggestions - if the suggestions box should be shown, default true
             */
            class_4.prototype.addSuggestion = function (suggestion, showSuggestions) {
                var _this = this;
                if (showSuggestions === void 0) { showSuggestions = true; }
                var element = this.renderer(suggestion);
                var value = suggestion.id || suggestion.value;
                var text = suggestion.completionLabel || suggestion.fieldLabel;
                element.dataset.value = value;
                element.dataset.text = text;
                element.dataset.tokenText = suggestion.fieldLabel;
                if (suggestion.type != null) {
                    element.dataset.type = suggestion.type;
                }
                if (suggestion.disabled) {
                    element.dataset.disabled = 'true';
                }
                element.addEventListener('click', function (_event) {
                    if (value == '_no_match_' || suggestion.disabled) {
                        return;
                    }
                    if (_this.parent.options.selectMode == SelectModes.SINGLE) {
                        if (element.classList.contains('token-autocomplete-suggestion-active')) {
                            _this.parent.select.clear(false);
                        }
                        else {
                            if (element.dataset.becomesToken !== 'false') {
                                _this.parent.select.addToken(value, suggestion.fieldLabel, suggestion.type, false);
                            }
                            _this.dispatchSuggestionSelectedEvent(element);
                        }
                    }
                    else {
                        _this.parent.select.clearCurrentInput();
                        if (element.classList.contains('token-autocomplete-suggestion-active')) {
                            var multiSelect = _this.parent.select;
                            multiSelect.removeTokenWithText(suggestion.fieldLabel);
                        }
                        else {
                            if (element.dataset.becomesToken !== 'false') {
                                _this.parent.select.addToken(value, suggestion.fieldLabel, suggestion.type, false);
                            }
                            _this.dispatchSuggestionSelectedEvent(element);
                        }
                    }
                    _this.clearSuggestions();
                    _this.hideSuggestions();
                });
                if (suggestion.disabled) {
                    element.classList.add('token-autocomplete-suggestion-disabled');
                }
                if (this.parent.tokenContainer.querySelector('.token-autocomplete-token[data-value="' + value + '"]') !== null) {
                    element.classList.add('token-autocomplete-suggestion-active');
                }
                this.suggestions.appendChild(element);
                if (showSuggestions) {
                    this.showSuggestions();
                }
                this.parent.log('added suggestion', suggestion);
            };
            return class_4;
        }()),
        __setFunctionName(_b, "Autocomplete"),
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