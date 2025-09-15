interface Token {
    value: string,
    text: string,
    type: string | null
}

interface Suggestion {
    id: string | null,
    value: string,
    fieldLabel: string,
    type: string | null,
    completionLabel: string | null,
    completionDescription: string | null,
    disabled: boolean | null
}

interface Options {
    name: string,
    selector: HTMLElement | string,
    noMatchesText: string | null,
    noMatchesCustomEntriesDescription: string | null,
    placeholderText: string | null,
    initialTokens: Array<Token> | null,
    initialSuggestions: Array<Suggestion> | null,
    tokenRenderer: TokenRenderer,
    selectMode: SelectModes,
    resolveUri: string,
    resolveUriBuilder: ResolveUriBuilder,
    suggestionsUri: string,
    suggestionsUriBuilder: SuggestionUriBuilder,
    suggestionRenderer: SuggestionRenderer,
    minCharactersForSuggestion: number,
    allowCustomEntries: boolean,
    readonly: boolean,
    optional: boolean,
    showClearButton: boolean,
    enableTabulator: boolean,
    showSuggestionsOnFocus: boolean,
    requestDelay: number
}

enum SelectModes {
    SINGLE, MULTI, SEARCH
}

interface SelectMode {
    addToken(suggestionValue: string | null, suggestionText: string | null, suggestionType: string | null, silent: boolean): void;

    handleInputAsValue(input: string): boolean;

    updateHasValue(): void;

    initEventListeners(): void;

    clear(silent: boolean): void;

    clearCurrentInput(): void;
}

interface SingleSelect extends SelectMode {
}

interface MultiSelect extends SelectMode {
    removeToken(token: HTMLSpanElement): void;

    removeLastToken(): void;

    removeTokenWithText(textContent: any): void;
}

interface Autocomplete {
    suggestions: any;

    initEventListeners(): void;

    requestSuggestions(value: string): void;

    highlightSuggestionAtPosition(arg0: number): void;

    addSuggestion(suggestion: Suggestion, showSuggestions: boolean): void;

    clearSuggestions(): void;

    showSuggestions(): void;

    hideSuggestions(): void;

    loadSuggestions(): void;

    areSuggestionsDisplayed(): boolean;

    areSuggestionsActive(): boolean;

    highlightSuggestion(arg0: Element): void;

    dispatchSuggestionSelectedEvent(_suggestion: HTMLElement): void;
}

interface TokenRenderer {
    (token: Token): HTMLElement;
}

interface SuggestionRenderer {
    (suggestion: Suggestion): HTMLElement;
}

interface SuggestionUriBuilder {
    (query: string): string;
}

interface ResolveUriBuilder {
    (value: string): string;
}

class TokenAutocomplete {

    KEY_BACKSPACE = 'Backspace';
    KEY_ENTER = 'Enter';
    KEY_TAB = 'Tab';
    KEY_UP = 'ArrowUp';
    KEY_DOWN = 'ArrowDown';
    KEY_LEFT = 'ArrowLeft';
    KEY_RIGHT = 'ArrowRight';
    KEY_ESC = 'Escape';

    options: Options;
    container: any;
    tokenContainer: HTMLDivElement;
    hiddenSelect: HTMLSelectElement;
    textInput: HTMLSpanElement;

    select: SelectMode;
    autocomplete: Autocomplete;

    defaults: Options = {
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
        resolveUriBuilder: (value) => {
            // We have to do this manually instead of using URL, as we can't be sure that a polyfill for IE11 is present
            const querySeparator = this.options.resolveUri.indexOf('?') >= 0 ? '&' : '?';
            return this.options.resolveUri + querySeparator + 'value=' + encodeURIComponent(value);
        },
        suggestionsUri: '',
        suggestionsUriBuilder: (query) => {
            // We have to do this manually instead of using URL, as we can't be sure that a polyfill for IE11 is present
            const querySeparator = this.options.suggestionsUri.indexOf('?') >= 0 ? '&' : '?';
            return this.options.suggestionsUri + querySeparator + 'query=' + encodeURIComponent(query);
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
    log: any;

    constructor(options: Options) {
        this.options = {...this.defaults, ...options};

        if (this.options.selector instanceof HTMLElement) {
            this.container = this.options.selector;
        } else {
            let passedContainer = document.querySelector(this.options.selector);
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
            this.textInput.addEventListener("paste", event => {
                event.preventDefault();
                if (event.clipboardData) {
                    //  Normal handling for modern browsers
                    const text = event.clipboardData?.getData("text/plain");
                    document.execCommand("insertHTML", false, text);
                } else {
                    // Fallback logic for IE11
                    const globalText = window.clipboardData?.getData("Text");
                    const range = document.getSelection()?.getRangeAt(0);
                    range?.insertNode(document.createTextNode(globalText));
                }
            });
        } else {
            this.container.classList.add('token-autocomplete-readonly');
        }
        this.tokenContainer.appendChild(this.textInput);


        this.container.appendChild(this.hiddenSelect);
        this.addHiddenEmptyOption();

        if (this.options.selectMode == SelectModes.MULTI) {
            this.select = new TokenAutocomplete.MultiSelect(this);
        } else if (this.options.selectMode == SelectModes.SEARCH) {
            this.select = new TokenAutocomplete.SearchMultiSelect(this);
        } else if (this.options.selectMode == SelectModes.SINGLE) {
            this.hiddenSelect.removeAttribute('multiple');
            this.select = new TokenAutocomplete.SingleSelect(this);
        }
        this.autocomplete = new TokenAutocomplete.Autocomplete(this);

        this.select.initEventListeners();
        this.autocomplete.initEventListeners();

        this.debug(false);

        if (Array.isArray(this.options.initialTokens)) {
            this.val(this.options.initialTokens);
            this.enrichInitialTokens().then(shouldUpdate => {
                if (shouldUpdate) {
                    this.val(this.options.initialTokens);
                }
            });
        }

        this.container.tokenAutocomplete = this as TokenAutocomplete;

        if (this.options.selectMode == SelectModes.SINGLE && !this.options.optional && this.val().length == 0) {
            this.autocomplete.loadSuggestions();
        }
    }

    async enrichInitialTokens() {
        if (this.options.resolveUri.length > 0 && this.options.initialTokens) {
            for await (const token of this.options.initialTokens) {
                // Fetch the data for each initial token and update its properties
                let resolveUri = this.options.resolveUriBuilder(token.value);
                await fetch(resolveUri).then(response => response.json()).then(data => {
                    token.text = data.text ?? token.text;
                    token.value = data.value ?? token.value;
                    token.type = data.type ?? token.type;
                });
            }
            return true;
        }
        return false;
    }

    /**
     * Searches the element given as a container for option elements and creates active tokens (when the option is marked selected)
     * and suggestions (all options found) from these. During this all found options are removed from the DOM.
     */
    parseTokensAndSuggestions() {
        let initialTokens: Array<Token> = [];
        let initialSuggestions: Array<Suggestion> = [];

        let options: NodeListOf<HTMLOptionElement> = this.container.querySelectorAll('option');

        options.forEach(option => {
            if (option.text != null) {
                if (option.hasAttribute('selected')) {
                    initialTokens.push({value: option.value, text: option.text, type: null});
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
            this.container.removeChild(option);
        });

        if (initialSuggestions.length > 0) {
            this.options.initialSuggestions = initialSuggestions;
            if (!this.options.optional && initialTokens.length == 0) {
                let firstSuggestion = initialSuggestions[0];
                initialTokens.push({
                    value: firstSuggestion.value, text: firstSuggestion.fieldLabel, type: firstSuggestion.type
                });
            }
        }

        if (initialTokens.length > 0) {
            this.options.initialTokens = initialTokens;
        }
    }

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
    val(value: Array<Token> | Token | null = null, silent: boolean = false): Array<string> {
        if (typeof value !== 'undefined' && value !== null) {
            this.select.clear(silent);
            this.addToken(value, silent);
        }

        let tokens: Array<string> = [];
        this.hiddenSelect.querySelectorAll('option').forEach(option => {
            if (option.dataset.value != null && option.dataset.value !== "") {
                tokens.push(option.dataset.value);
            }
        });
        return tokens;
    }


    /**
     * Adds the given tokens to the field.
     *
     * The current tokens are only added when a value parameter is given.
     *
     * @param {(Array<Token>|Token)} value - either the name of a single token or a list of tokens to create
     * @param {boolean} silent - whether appropriate events should be triggered when changing tokens or not
     */
    addToken(value: Array<Token> | Token, silent: boolean = false) {
        if (Array.isArray(value)) {
            value.forEach(token => {
                if (typeof token === 'object') {
                    this.select.addToken(token.value, token.text, token.type, silent);
                }
            });
        } else {
            this.select.addToken(value.value, value.text, value.type, silent);
        }
    }

    /**
     * Returns the current text the user has input which is not converted into a token.
     */
    getCurrentInput() {
        return this.textInput.textContent || '';
    }

    setCurrentInput(input: string, silent: boolean) {
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
    }

    addHiddenOption(tokenValue: string, tokenText: string, tokenType: string | null, isLiveEntry: boolean = false) {
        tokenValue = tokenValue.trim();
        tokenText = tokenText.trim();
        let _emptyToken = this.hiddenSelect.querySelector('.empty-token');
        if (_emptyToken) {
            this.hiddenSelect.removeChild(_emptyToken);
        }
        let _existingLiveEntry = this.hiddenSelect.querySelector('.token-autocomplete-live-entry');
        if (_existingLiveEntry) {
            this.hiddenSelect.removeChild(_existingLiveEntry);
        }
        let _existingOption = this.findOptionWithValue(tokenValue);
        if (_existingOption) {
            this.hiddenSelect.removeChild(_existingOption);
        }
        const option = document.createElement('option');
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
    }

    findOptionWithValue(optionValue: string) {
        for (let i = 0; i < this.hiddenSelect.options.length; i++) {
            let option = this.hiddenSelect.options[i];
            if (option.value === optionValue) {
                return option;
            }
        }
        return null;
    }

    addHiddenEmptyOption() {
        let _emptyToken = this.hiddenSelect.querySelector('.empty-token');
        if (_emptyToken) {
            _emptyToken.setAttribute('selected', 'true');
        } else {
            const _newOption = document.createElement('option');
            _newOption.text = '';
            _newOption.value = '';
            _newOption.selected = true;
            _newOption.classList.add('empty-token');
            this.hiddenSelect.add(_newOption);
        }
    }

    setPlaceholderText(placeholderText: string | undefined) {
        this.textInput.dataset.placeholder = placeholderText;
    }

    debug(state: boolean) {
        if (state) {
            this.log = console.log.bind(window.console);
        } else {
            this.log = () => {
                // Intentionally left empty to only log when debugging is enabled.
            }
        }
    }

    static MultiSelect = class implements MultiSelect {

        parent: TokenAutocomplete;
        container: any;
        tokenContainer: HTMLDivElement;
        options: Options;
        renderer: TokenRenderer;

        constructor(parent: TokenAutocomplete) {
            this.parent = parent;
            this.container = parent.container;
            this.tokenContainer = parent.tokenContainer;
            this.options = parent.options;
            this.renderer = parent.options.tokenRenderer;

            if (this.options.showClearButton) {
                let clearButton = document.createElement('span');
                clearButton.classList.add('token-autocomplete-delete-button');
                this.container.appendChild(clearButton);
            }
        }

        clearCurrentInput(ignored: boolean = false): void {
            const previousInput = this.parent.getCurrentInput();
            this.parent.textInput.textContent = '';

            this.parent.log('cleared input', previousInput);
        }

        initEventListeners(): void {
            const parent = this.parent;
            if (parent.options.readonly) {
                return;
            }

            let isComposing = false;
            parent.textInput.addEventListener('compositionstart', () => {
                isComposing = true;
            })
            parent.textInput.addEventListener('compositionend', event => {
                isComposing = false;

                // handles hitting ENTER on GBoard, which uses composition events instead of individual key triggers
                let inputString = event.data;
                if (inputString.charAt(inputString.length - 1) === "\n") {
                    event.preventDefault();
                    this.handleInput(parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted'));
                }
            })

            parent.textInput.addEventListener('keydown', event => {
                if (isComposing) return;

                if (event.key == parent.KEY_ENTER || (event.key == parent.KEY_TAB && parent.options.enableTabulator && parent.autocomplete.areSuggestionsDisplayed() && parent.autocomplete.suggestions.childNodes.length == 1)) {
                    event.preventDefault();

                    let highlightedSuggestion = parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');

                    if (parent.options.enableTabulator && highlightedSuggestion == null && event.key == parent.KEY_TAB && parent.autocomplete.areSuggestionsDisplayed()) {
                        highlightedSuggestion = parent.autocomplete.suggestions.firstChild;
                    }

                    this.handleInput(highlightedSuggestion);
                } else if (parent.getCurrentInput() === '' && event.key == parent.KEY_BACKSPACE) {
                    event.preventDefault();
                    this.removeLastToken();
                }
                if ((event.key == parent.KEY_DOWN || event.key == parent.KEY_UP) && parent.autocomplete.suggestions.childNodes.length > 0) {
                    event.preventDefault();
                }
            });

            parent.textInput.addEventListener('keyup', () => this.updateHasValue());

            if (!(this instanceof TokenAutocomplete.SearchMultiSelect)) {
                // We don't want to trigger a search when the user focuses out of the input field in a search field.
                // This should only try to apply the current input text as value in normal multi-selects.
                parent.textInput.addEventListener('focusout', (event: FocusEvent) => {
                    // Using setTimeout here seems hacky on first sight but ensures proper order of events / handling.
                    // We first want to handle a click on a suggestion (when one is made) before hiding the suggestions on focusout of the input.
                    // Not doing so could mean the suggestion is hidden before the click is handled und thus resulting in not being selected.
                    // This depends on the order in which a browser handles different events and when it sets the active pseudo-selector on clicked events (Firefox for example)
                    setTimeout(() => {
                        if (parent.autocomplete.areSuggestionsActive()) {
                            return;
                        }
                        const input = this.parent.getCurrentInput();
                        if (input != '' && !this.handleInputAsValue(input)) {
                            this.container.dispatchEvent(new CustomEvent('input-ignored', {
                                detail: {
                                    input: input
                                }
                            }));
                        }
                    }, 0);
                });
            }

            if (this.options.showClearButton) {
                parent.container.querySelector('.token-autocomplete-delete-button')?.addEventListener('click', () => {
                    this.clear(true);
                    this.clearCurrentInput();
                    this.updateHasValue();

                    this.container.dispatchEvent(new CustomEvent('input-cleared'));
                });
            }
        }

        handleInput(highlightedSuggestion: any): void {
            if (highlightedSuggestion !== null) {
                this.clearCurrentInput();
                if (highlightedSuggestion.classList.contains('token-autocomplete-suggestion-active')) {
                    this.removeTokenWithText(highlightedSuggestion.dataset.tokenText);
                } else {
                    if (highlightedSuggestion.dataset.becomesToken !== 'false') {
                        this.addToken(highlightedSuggestion.dataset.value, highlightedSuggestion.dataset.tokenText, highlightedSuggestion.dataset.type, false);
                    }
                    this.parent.autocomplete.dispatchSuggestionSelectedEvent(highlightedSuggestion);
                }
            } else {
                this.handleInputAsValue(this.parent.getCurrentInput());
            }
            this.parent.autocomplete.clearSuggestions();
            this.parent.autocomplete.hideSuggestions();
        }

        /**
         * Updates the 'token-autocomplete-has-value' class of this MultiSelect autocomplete.
         */
        updateHasValue(): void {
            if (this.parent.getCurrentInput() === '' && this.parent.val().length === 0) {
                this.container.classList.remove('token-autocomplete-has-value');
            } else {
                this.container.classList.add('token-autocomplete-has-value');
            }
        }

        /**
         * Adds the current user input as a net token and resets the input area so new text can be entered.
         *
         * @param {string} input - the actual input the user entered
         * @returns {boolean} - whether the input was handled as a value (true) or discarded (false)
         */
        handleInputAsValue(input: string): boolean {
            if (input != '' && this.parent.options.allowCustomEntries) {
                this.clearCurrentInput();
                this.addToken(input, input, null);
                return true;
            }
            if (this.parent.autocomplete.suggestions.childNodes.length === 1 && this.parent.autocomplete.suggestions.childNodes[0].dataset.value != '_no_match_') {
                this.parent.autocomplete.suggestions.firstChild.click();
                return true;
            } else {
                this.clearCurrentInput();
                return false;
            }
        }

        /**
         * Adds a token with the specified name to the list of currently present tokens displayed to the user and the hidden select.
         *
         * @param {string} tokenValue - the actual value of the token to create
         * @param {string} tokenText - the name of the token to create
         * @param {string} tokenType - the type of the token to create
         * @param {boolean} silent - whether an appropriate event should be triggered
         */
        addToken(tokenValue: string | null, tokenText: string | null, tokenType: string | null, silent: boolean = false) {
            if (tokenValue === null || tokenText === null || tokenValue === '_no_match_') {
                return;
            }

            tokenValue = tokenValue.trim();
            tokenText = tokenText.trim();

            this.parent.addHiddenOption(tokenValue, tokenText, tokenType);

            let addedToken = {
                value: tokenValue,
                text: tokenText,
                type: tokenType
            };

            let element = this.renderer(addedToken);

            element.querySelector('.token-autocomplete-delete-button')?.addEventListener('click', () => {
                this.removeToken(element);
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
        }

        /**
         * Completely clears the currently present tokens from the field.
         */
        clear(silent: boolean = false) {
            let tokens: NodeListOf<HTMLElement> = this.tokenContainer.querySelectorAll('.token-autocomplete-token');

            tokens.forEach(token => this.removeToken(token, silent));
        }

        /**
         * Removes the last token in the list of currently present token. This is the last added token next to the input field.
         */
        removeLastToken() {
            let tokens = this.tokenContainer.querySelectorAll('.token-autocomplete-token');
            let token = tokens[tokens.length - 1] as HTMLElement;
            if (token) {
                this.removeToken(token);
            }
        }

        /**
         * Removes the specified token from the list of currently present tokens.
         *
         * @param {Element} token - the token to remove
         * @param {boolean} silent - whether an appropriate event should be triggered
         */
        removeToken(token: HTMLElement, silent: boolean = false) {
            this.tokenContainer.removeChild(token);

            let tokenText = token.dataset.text;
            let hiddenOption = this.parent.hiddenSelect.querySelector('option[data-text="' + TokenAutocomplete.escapeQuotes(tokenText) + '"]');
            hiddenOption?.parentElement?.removeChild(hiddenOption);

            let addedToken = {
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
        }

        removeTokenWithText(tokenText: string | null) {
            if (tokenText === null) {
                return;
            }
            let token = this.tokenContainer.querySelector('.token-autocomplete-token[data-text="' + TokenAutocomplete.escapeQuotes(tokenText) + '"]') as HTMLElement;
            if (token !== null) {
                this.removeToken(token);
            }
        }

        static defaultRenderer: TokenRenderer = function (token: Token): HTMLElement {
            const chip = document.createElement('span');
            chip.classList.add('token-autocomplete-token');
            chip.dataset.text = token.text;
            chip.dataset.value = token.value;
            if (token.type != null) {
                chip.dataset.type = token.type;
            }
            chip.textContent = token.text;

            let deleteToken = document.createElement('span');
            deleteToken.classList.add('token-autocomplete-delete-button');
            chip.appendChild(deleteToken);

            return chip;
        }

        static defaultReadonlyRenderer: TokenRenderer = function (token: Token): HTMLElement {
            const chip = document.createElement('span');
            chip.classList.add('token-autocomplete-token');
            chip.dataset.text = token.text;
            chip.dataset.value = token.value;
            if (token.type != null) {
                chip.dataset.type = token.type;
            }
            chip.textContent = token.text;

            return chip;
        }
    }

    static SingleSelect = class implements SingleSelect {

        parent: TokenAutocomplete;
        container: any;
        toggleButton: HTMLButtonElement;
        options: Options;
        renderer: TokenRenderer;
        previousValue: any;
        previousText: any;
        previousType: any;

        constructor(parent: TokenAutocomplete) {
            this.parent = parent;
            this.container = parent.container;
            this.options = parent.options;

            this.container.classList.add('token-autocomplete-singleselect');
            this.parent.textInput.tabIndex = 0;
            if (this.options.optional) {
                let clearButton = document.createElement('span');
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
        clear(silent: boolean, keepPreviousValue: boolean = true): void {
            if (this.options.readonly) {
                return;
            }
            let tokenText = this.parent.textInput.textContent;
            let hiddenOption = this.parent.hiddenSelect.querySelector('option[data-text="' + TokenAutocomplete.escapeQuotes(tokenText) + '"]') as HTMLElement;

            this.container.classList.remove('token-autocomplete-has-value');

            const previousValue = hiddenOption?.dataset.value;
            const previousText = hiddenOption?.dataset.text;
            const previousType = hiddenOption?.dataset.type;
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
            } else {
                // We should reset these fields, so they are not used to restore the previously selected value
                // when the focusout event is handled after the click event on the suggestion.
                delete this.previousValue;
                delete this.previousText;
                delete this.previousType;
                if (this.parent.options.placeholderText != null) {
                    this.parent.textInput.dataset.placeholder = this.parent.options.placeholderText;
                }
            }
            hiddenOption?.parentElement?.removeChild(hiddenOption);
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
        }

        /**
         * Adds the current user input as a net token and resets the input area so new text can be entered.
         *
         * @param {string} input - the actual input the user entered
         * @returns {boolean} - whether the input was handled as a value (true) or discarded (false)
         */
        handleInputAsValue(input: string): boolean {
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
        }

        clearCurrentInput(): void {
            this.clear(true);
        }

        /**
         * Updates the 'token-autocomplete-has-value' class of this SingleSelect autocomplete.
         */
        updateHasValue(): void {
            if (this.parent.getCurrentInput() === '' && this.parent.val().length === 0) {
                this.container.classList.remove('token-autocomplete-has-value');
            } else {
                this.container.classList.add('token-autocomplete-has-value');
            }
        }

        addToken(tokenValue: string | null, tokenText: string | null, tokenType: string | null, silent: boolean): void {
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
        }

        initEventListeners(): void {
            const parent = this.parent;
            if (parent.options.readonly) {
                return;
            }

            parent.textInput.addEventListener('compositionend', event => {
                // handles hitting ENTER on GBoard, which uses composition events instead of individual key triggers
                let inputString = event.data;
                if (inputString.charAt(inputString.length - 1) === "\n") {
                    event.preventDefault();
                    this.handleInput(parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted'));
                }
            })

            parent.textInput.addEventListener('keydown', event => {
                if (event.key == parent.KEY_ENTER || (event.key == parent.KEY_TAB && parent.options.enableTabulator && parent.autocomplete.areSuggestionsDisplayed() && parent.autocomplete.suggestions.childNodes.length == 1)) {
                    event.preventDefault();

                    let highlightedSuggestion = parent.autocomplete.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');

                    if (parent.options.enableTabulator && highlightedSuggestion == null && event.key == parent.KEY_TAB && parent.autocomplete.areSuggestionsDisplayed()) {
                        highlightedSuggestion = parent.autocomplete.suggestions.firstChild;
                    }

                    this.handleInput(highlightedSuggestion);
                }
                if ((event.key == parent.KEY_DOWN || event.key == parent.KEY_UP) && parent.autocomplete.suggestions.childNodes.length > 0) {
                    event.preventDefault();
                }
            });

            const focusInput = () => {
                if (!parent.autocomplete.areSuggestionsDisplayed() && parent.options.showSuggestionsOnFocus) {
                    parent.autocomplete.showSuggestions();
                    parent.autocomplete.loadSuggestions();
                }
                // move the cursor into the editable div
                const selection = window.getSelection();
                const range = document.createRange();
                selection?.removeAllRanges();
                range.selectNodeContents(parent.textInput);
                range.collapse(false);
                selection?.addRange(range);
                parent.textInput.focus();
            };

            parent.textInput.addEventListener('click', () => focusInput());

            parent.textInput.addEventListener('focusout', (event: FocusEvent) => {
                if (event.relatedTarget === this.toggleButton && parent.autocomplete.areSuggestionsDisplayed()) {
                    // If the focus is moved to the toggle button, we mark it so the click handler does not set focus again.
                    this.toggleButton.dataset.inputWasFocused = 'true';
                }
                // Using setTimeout here seems hacky on first sight but ensures proper order of events / handling.
                // We first want to handle a click on a suggestion (when one is made) before hiding the suggestions on focusout of the input.
                // Not doing so could mean the suggestion is hidden before the click is handled und thus resulting in not being selected.
                // This depends on the order in which a browser handles different events and when it sets the active pseudo-selector on clicked events (Firefox for example)
                setTimeout(() => {
                    if (parent.autocomplete.areSuggestionsActive()) {
                        return;
                    }
                    const input = this.parent.getCurrentInput();
                    if (this.parent.val().length !== 0 && this.parent.val()[0] !== '') {
                        return;
                    }
                    if (input != '') {
                        if (!this.handleInputAsValue(input)) {
                            this.container.dispatchEvent(new CustomEvent('input-ignored', {
                                detail: {
                                    input: input
                                }
                            }));
                        }
                        return;
                    }
                    if (this.previousValue) {
                        this.addToken(this.previousValue, this.previousText, this.previousType, true);
                    }
                }, 0);

            });
            parent.container.querySelector('.token-autocomplete-delete-button')?.addEventListener('click', () => {
                this.clear(false, false);
            });

            this.toggleButton.addEventListener('click', () => {
                if (this.toggleButton.dataset.inputWasFocused === 'true') {
                    // The focus was moved to the toggle button, so we do not want to focus the input again.
                    delete this.toggleButton.dataset.inputWasFocused;
                    this.toggleButton.blur();
                } else {
                    // If the input is not focused, we want to focus it and show the suggestions.
                    focusInput();
                }
            });
        }

        handleInput(highlightedSuggestion: any): void {
            if (highlightedSuggestion !== null) {
                this.addToken(highlightedSuggestion.dataset.value, highlightedSuggestion.dataset.tokenText, highlightedSuggestion.dataset.type, false);
            } else {
                this.handleInputAsValue(this.parent.getCurrentInput());
            }
            this.parent.autocomplete.clearSuggestions();
            this.parent.autocomplete.hideSuggestions();
        }
    }

    static SearchMultiSelect = class extends TokenAutocomplete.MultiSelect {
        /**
         * Instead of adding the custom user input as a token and handling it as a filter we let it remain in the input
         * area and instead send an event so the user search request can be handled / executed.
         *
         * @param {string} input - the actual input the user entered
         * @returns {boolean} - whether the input was handled as a value (true) or discarded (false)
         */
        handleInputAsValue(input: string): boolean {
            this.container.dispatchEvent(new CustomEvent('query-changed', {
                detail: {
                    query: input
                }
            }));
            return true;
        }
    }

    static Autocomplete = class implements Autocomplete {

        parent: TokenAutocomplete;
        container: any;
        options: Options;
        suggestions: HTMLUListElement;
        renderer: SuggestionRenderer;
        request: XMLHttpRequest | null;
        timeout: number | undefined;
        scrollParent: HTMLElement | Window | null = null;
        private hideHandler = () => {
            this.hideSuggestions();
        }

        constructor(parent: TokenAutocomplete) {
            this.parent = parent;
            this.container = parent.container;
            this.options = parent.options;
            this.renderer = parent.options.suggestionRenderer;

            this.suggestions = document.createElement('ul');
            this.suggestions.id = this.container.id + '-suggestions';
            this.suggestions.classList.add('token-autocomplete-suggestions');
            this.container.appendChild(this.suggestions);
        }

        initEventListeners() {
            if (this.parent.options.readonly) {
                return;
            }
            this.parent.textInput.addEventListener('keyup', event => {
                if (event.key == this.parent.KEY_ESC) {
                    this.hideSuggestions();
                    this.parent.textInput.blur();
                    return;
                }
                if (event.key == this.parent.KEY_UP && this.suggestions.childNodes.length > 0) {
                    event.preventDefault();
                    let highlightedSuggestion = this.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                    if (highlightedSuggestion == null) {
                        // highlight last entry and scroll to bottom
                        let bottomSuggestion = this.suggestions.lastChild;
                        while (bottomSuggestion != null && TokenAutocomplete.shouldIgnoreSuggestion(bottomSuggestion)) {
                            bottomSuggestion = bottomSuggestion.previousSibling;
                        }
                        if (bottomSuggestion != null) {
                            this.highlightSuggestion(bottomSuggestion as Element);
                            this.suggestions.scrollTop = this.suggestions.scrollHeight;
                        }
                        return;
                    }
                    let aboveSuggestion = highlightedSuggestion.previousSibling;
                    while (aboveSuggestion != null && TokenAutocomplete.shouldIgnoreSuggestion(aboveSuggestion)) {
                        aboveSuggestion = aboveSuggestion.previousSibling;
                    }
                    if (aboveSuggestion != null) {
                        // if the suggestions is above the scroll position, scroll to the suggestion
                        let suggestionTop = (aboveSuggestion as HTMLElement).offsetTop;
                        if (this.suggestions.scrollTop > suggestionTop) {
                            this.suggestions.scrollTop = suggestionTop;
                        }
                        this.highlightSuggestion(aboveSuggestion as Element);
                    } else {
                        highlightedSuggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                    }
                    return;
                }
                if (event.key == this.parent.KEY_DOWN && this.suggestions.childNodes.length > 0) {
                    event.preventDefault();
                    let highlightedSuggestion = this.suggestions.querySelector('.token-autocomplete-suggestion-highlighted');
                    if (highlightedSuggestion == null) {
                        // highlight first entry and scroll to top
                        let topSuggestion = this.suggestions.firstChild;
                        while (topSuggestion != null && TokenAutocomplete.shouldIgnoreSuggestion(topSuggestion)) {
                            topSuggestion = topSuggestion.nextSibling;
                        }
                        if (topSuggestion != null) {
                            this.highlightSuggestion(topSuggestion as Element);
                            this.suggestions.scrollTop = 0;
                        }
                        return;
                    }
                    let belowSuggestion = highlightedSuggestion?.nextSibling;
                    while (belowSuggestion != null && TokenAutocomplete.shouldIgnoreSuggestion(belowSuggestion)) {
                        belowSuggestion = belowSuggestion.nextSibling;
                    }
                    if (belowSuggestion != null) {
                        // if the suggestions is not completely visible, scroll until the suggestion is at the bottom
                        let suggestionBottom = (belowSuggestion as HTMLElement).offsetTop + (belowSuggestion as HTMLElement).offsetHeight;
                        if (this.suggestions.scrollTop + this.suggestions.clientHeight < suggestionBottom) {
                            this.suggestions.scrollTop = suggestionBottom - this.suggestions.clientHeight;
                        }
                        this.highlightSuggestion(belowSuggestion as Element);
                    } else {
                        highlightedSuggestion.classList.remove('token-autocomplete-suggestion-highlighted');
                    }
                    return;
                }
                if (event.key == this.parent.KEY_LEFT || event.key == this.parent.KEY_RIGHT || event.key == this.parent.KEY_ENTER || event.key == this.parent.KEY_TAB) {
                    // We don't want to re-trigger the autocompletion when the user navigates the cursor inside the input.
                    return;
                }
                this.loadSuggestions();
            });
            this.parent.textInput.addEventListener('focusout', () => {
                // Using setTimeout here seems hacky on first sight but ensures proper order of events / handling.
                // We first want to handle a click on a suggestion (when one is made) before hiding the suggestions on focusout of the input.
                // Not doing so could mean the suggestion is hidden before the click is handled und thus resulting in not being selected.
                // This depends on the order in which a browser handles different events and when it sets the active pseudo-selector on clicked events (Firefox for example)
                setTimeout(() => {
                    if (this.areSuggestionsActive()) {
                        return;
                    }
                    this.hideSuggestions();
                }, 0);
            });
            this.parent.textInput.addEventListener('focusin', () => {
                if (this.options.showSuggestionsOnFocus) {
                    this.loadSuggestions();
                    this.showSuggestions();
                }
            });
        }

        loadSuggestions() {
            let value = this.parent.getCurrentInput();

            if (this.parent.options.selectMode == SelectModes.SINGLE) {
                if (!this.parent.textInput.isContentEditable) {
                    this.parent.select.clear(true);
                    value = "";
                }
            } else if (value.length < this.parent.options.minCharactersForSuggestion) {
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
                this.parent.options.initialSuggestions.forEach(suggestion => {
                    if (typeof suggestion !== 'object') {
                        // The suggestion is of wrong type and therefore ignored.
                        return;
                    }
                    let text = suggestion.fieldLabel;
                    if (value.length == 0 && this.options.selectMode == SelectModes.SINGLE && !this.options.optional && !this.areSuggestionsDisplayed()) {
                        this.addSuggestion(suggestion, false);
                    } else if (value.localeCompare(text.slice(0, value.length), undefined, {sensitivity: 'base'}) === 0) {
                        // The suggestion starts with the query text the user entered and will be displayed.
                        this.addSuggestion(suggestion);
                    }
                });
                if (value.length >= this.parent.options.minCharactersForSuggestion) {
                    const hasExactMatch = this.suggestions.querySelector(`li[data-value='${value}']:not([data-type='_no_match_']),li[data-text='${value}']:not([data-type='_no_match_'])`);
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

                    } else if (this.suggestions.childNodes.length == 0 && this.parent.options.noMatchesText) {
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
        }

        /**
         * Hides the suggestions dropdown from the user.
         */
        hideSuggestions() {
            // as the suggestions will be re-shown if a pending request is executed, we abort them if we want to hide
            this.abortPendingRequest();
            this.container.classList.remove('token-autocomplete-suggestions-displayed');
            this.suggestions.style.display = '';

            let _highlightedSuggestions = this.suggestions.querySelectorAll('li.token-autocomplete-suggestion-highlighted');
            _highlightedSuggestions.forEach(_suggestion => {
                _suggestion.classList.remove('token-autocomplete-suggestion-highlighted');
            });
            window.removeEventListener('resize', this.hideHandler);
            this.scrollParent?.removeEventListener('scroll', this.hideHandler);
        }

        /**
         * Shows the suggestions dropdown to the user.
         */
        showSuggestions() {
            this.container.classList.add('token-autocomplete-suggestions-displayed');
            this.suggestions.style.display = 'block';

            this.updateSuggestionsPosition();
            window.addEventListener('resize', this.hideHandler);
            this.scrollParent = this.findScrollParent(this.container);
            this.scrollParent?.addEventListener('scroll', this.hideHandler);
        }

        updateSuggestionsPosition() {
            if (!this.areSuggestionsDisplayed()) {
                return;
            }
            const containerBox = this.container.getBoundingClientRect();
            const suggestionsHeight = this.suggestions.offsetHeight;
            const viewportHeight = window.innerHeight;
            const spaceBelow = viewportHeight - containerBox.bottom;

            this.suggestions.style.left = `${containerBox.left}px`;
            this.suggestions.style.width = `${containerBox.width}px`;
            if (spaceBelow < suggestionsHeight) {
                this.suggestions.style.bottom = `${viewportHeight - containerBox.top}px`;
                this.suggestions.style.top = 'initial';
            } else {
                this.suggestions.style.top = `${containerBox.bottom}px`;
                this.suggestions.style.bottom = 'initial';
            }
        }

        findScrollParent(_scrollingElement: HTMLElement | null): HTMLElement | Window | null {
            if (_scrollingElement == null) {
                return window;
            }
            if (_scrollingElement === document.documentElement || _scrollingElement === document.body) {
                return window;
            }
            const overflowY = window.getComputedStyle(_scrollingElement).overflowY;
            const isScrollable = overflowY !== 'visible' && overflowY !== 'hidden';
            if (isScrollable && _scrollingElement.scrollHeight >= _scrollingElement.clientHeight) {
                return _scrollingElement;
            }
            return this.findScrollParent(_scrollingElement.parentNode as HTMLElement);
        }

        areSuggestionsDisplayed() {
            return this.suggestions.style.display === 'block';
        }

        highlightSuggestionAtPosition(index: number) {
            let _suggestions = this.suggestions.querySelectorAll('li');
            _suggestions.forEach(_suggestion => {
                _suggestion.classList.remove('token-autocomplete-suggestion-highlighted');
            })
            _suggestions[index].classList.add('token-autocomplete-suggestion-highlighted');
        }

        highlightSuggestion(_suggestion: Element) {
            this.suggestions.querySelectorAll('li.token-autocomplete-suggestion-highlighted').forEach(_highlightedSuggestion => {
                _highlightedSuggestion.classList.remove('token-autocomplete-suggestion-highlighted');
            })
            _suggestion.classList.add('token-autocomplete-suggestion-highlighted');
        }

        /**
         * Checks for the presence of a suggestion that is currently clicked via mouse (active).
         */
        areSuggestionsActive() {
            return !!this.suggestions.querySelector('li:active');
        }

        /**
         * Removes all previous suggestions from the dropdown.
         */
        clearSuggestions() {
            this.abortPendingRequest();
            this.suggestions.innerHTML = '';
        }

        /**
         * Aborts currently in progress or scheduled suggestions requests.
         */
        abortPendingRequest() {
            this.request?.abort();
            clearTimeout(this.timeout);
        }

        /**
         * Loads suggestions matching the given query from the rest service behind the URI given as an option while initializing the field.
         *
         * @param query the query to search suggestions for
         */
        requestSuggestions(query: string) {
            clearTimeout(this.timeout);
            if (!this.timeout) {
                this.debouncedRequestSuggestions.call(this, query);
                this.timeout = window.setTimeout(() => {
                    delete this.timeout;
                }, this.parent.options.requestDelay);
            } else {
                this.timeout = window.setTimeout(() => {
                    delete this.timeout;
                    this.debouncedRequestSuggestions.call(this, query);
                }, this.parent.options.requestDelay);
            }
        }

        dispatchSuggestionSelectedEvent(_suggestion: HTMLElement) {
            _suggestion.dispatchEvent(new CustomEvent('suggestion-selected', {
                detail: {
                    value: _suggestion.dataset.value,
                    text: _suggestion.dataset.text,
                    type: _suggestion.dataset.type || null
                }
            }));
        }

        debouncedRequestSuggestions(query: string) {
            if (this.request != null && this.request.readyState) {
                this.request.abort();
            }

            const request = new XMLHttpRequest();
            this.request = request;
            this.request.onload = () => {
                this.request = null;

                this.clearSuggestions();

                let answer = request.response;
                // IE 11 doesn't properly respect content type header, need to parse json string by hand.
                if (typeof answer === 'string') {
                    answer = JSON.parse(answer);
                }

                if (Array.isArray(answer.completions)) {
                    if (this.parent.val().length == 0 && answer.completions.length > 0 && this.options.selectMode == SelectModes.SINGLE && !this.options.optional && !this.areSuggestionsDisplayed()) {
                        answer.completions.forEach((suggestion: Suggestion) => this.addSuggestion(suggestion, false));
                        let firstSuggestion = answer.completions[0] as Suggestion;
                        let value = firstSuggestion.id || firstSuggestion.value;
                        this.parent.select.addToken(value, firstSuggestion.fieldLabel, firstSuggestion.type, true);
                        return;
                    }
                    answer.completions.forEach((suggestion: Suggestion) => this.addSuggestion(suggestion));

                    const value = this.parent.getCurrentInput();
                    if (value.length >= this.parent.options.minCharactersForSuggestion) {
                        const hasExactMatch = this.suggestions.querySelector(`li[data-value='${value}']:not([data-type='_no_match_']),li[data-text='${value}']:not([data-type='_no_match_'])`);
                        if (!hasExactMatch && this.parent.options.allowCustomEntries && this.parent.options.noMatchesCustomEntriesDescription) {
                            this.addSuggestion({
                                id: null,
                                value: query,
                                fieldLabel: query,
                                type: '_no_match_',
                                completionDescription: this.parent.options.noMatchesCustomEntriesDescription,
                                completionLabel: null,
                                disabled: false
                            });
                        } else if (this.suggestions.childNodes.length == 0 && this.parent.options.noMatchesText) {
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
            let suggestionsUri = this.options.suggestionsUriBuilder(query);
            this.request.open('GET', suggestionsUri, true);
            this.request.responseType = 'json';
            this.request.setRequestHeader('Content-type', 'application/json');
            this.request.send();
        }

        /**
         * Adds a suggestion with the given text matching the users input to the dropdown.
         *
         * @param {string} suggestion - the metadata of the suggestion that should be added
         * @param showSuggestions - if the suggestions box should be shown, default true
         */
        addSuggestion(suggestion: Suggestion, showSuggestions = true) {
            let element = this.renderer(suggestion);

            let value = suggestion.id || suggestion.value;
            let text = suggestion.completionLabel || suggestion.fieldLabel;

            element.dataset.value = value;
            element.dataset.text = text;
            element.dataset.tokenText = suggestion.fieldLabel;
            if (suggestion.type != null) {
                element.dataset.type = suggestion.type;
            }
            if (suggestion.disabled) {
                element.dataset.disabled = 'true';
            }

            element.addEventListener('click', (_event: Event) => {
                if (value == '_no_match_' || suggestion.disabled) {
                    return;
                }
                if (this.parent.options.selectMode == SelectModes.SINGLE) {
                    if (element.classList.contains('token-autocomplete-suggestion-active')) {
                        this.parent.select.clear(false);
                    } else {
                        if (element.dataset.becomesToken !== 'false') {
                            this.parent.select.addToken(value, suggestion.fieldLabel, suggestion.type, false);
                        }
                        this.dispatchSuggestionSelectedEvent(element);
                    }
                } else {
                    this.parent.select.clearCurrentInput();
                    if (element.classList.contains('token-autocomplete-suggestion-active')) {
                        let multiSelect = this.parent.select as MultiSelect;
                        multiSelect.removeTokenWithText(suggestion.fieldLabel);
                    } else {
                        if (element.dataset.becomesToken !== 'false') {
                            this.parent.select.addToken(value, suggestion.fieldLabel, suggestion.type, false);
                        }
                        this.dispatchSuggestionSelectedEvent(element);
                    }
                }
                this.clearSuggestions();
                this.hideSuggestions();
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
        }

        static defaultRenderer: SuggestionRenderer = function (suggestion: Suggestion): HTMLElement {
            let option = document.createElement('li');
            option.textContent = suggestion.completionLabel || suggestion.fieldLabel;

            if (suggestion.completionDescription) {
                let description = document.createElement('small');
                description.textContent = suggestion.completionDescription;
                description.classList.add('token-autocomplete-suggestion-description');
                option.appendChild(description);
            }

            return option;
        }
    }

    static escapeQuotes(text: string | null | undefined): string {
        return text?.replace(/\x22/g, '\\\x22') ?? '';
    }

    static shouldIgnoreSuggestion(suggestion: Node) {
        if (suggestion instanceof HTMLElement) {
            return suggestion.dataset.disabled === 'true';
        }
        return true;
    }
}
