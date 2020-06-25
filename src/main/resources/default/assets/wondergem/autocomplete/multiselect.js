/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

var multiSelect = function (args) {
    var $select = $('#' + args.id + '-suggestions-select');

    var createSuggestionsObject = function () {
        var allSuggestions = [];
        var initialSelection = [];

        for (var i = 0; i < $select[0].options.length; i++) {
            var option = $select[0].options[i];
            var token = {
                label: option.text,
                value: option.value
            };
            if (token.label === "" && token.value === "") {
                continue;
            }
            allSuggestions.push(token);
            if (option.selected) {
                initialSelection.push(token);
            }
        }

        return {
            getAllSuggestions: function () {
                return allSuggestions;
            },
            getInitialSelection: function () {
                return initialSelection;
            },
            addSuggestion: function (token) {
                allSuggestions.push(token)
            },
            forEachMatchingSuggestion: function (query, callback) {
                var found = undefined;
                $.each(allSuggestions, function (i, element) {
                    if (element.label.toLowerCase().indexOf(query.toLowerCase()) !== -1 ||
                        element.value.toLowerCase().indexOf(query.toLowerCase()) !== -1) {
                        if (callback) {
                            callback(element);
                        }
                    }
                });
                return found;
            },
            getTokenForValue: function (value) {
                var token = undefined;
                $.each(allSuggestions, function (i, element) {
                    if (String(element.value) === String(value)) {
                        token = element;
                    }
                });
                return token;
            }
        }
    }

    /**
     * Updates the original select object to represent the active tokens as we use it to transfer the selection.
     */
    var updateSelectObject = function () {
        $select.children('option').remove();

        var tokens = tokenfield.getTokens();

        tokens.forEach(function (token) {
            $('<option></option>').text(token.label).val(token.value).attr('selected', 'selected').appendTo($select);
        });

        suggestions.getAllSuggestions().forEach(function (token) {
            if ($select.children('option[value="' + token.value + '"]').length === 0) {
                $('<option></option>').text(token.label).val(token.value).appendTo($select);
            }
        });

        // Needed to still transfer the field if nothing is selected
        if (tokens.length === 0) {
            $('<option></option>').val('').attr('selected', 'selected').appendTo($select);
        }
    }

    var autocompleteTemplates = {
        basic:
            '<div tabindex="0" class="autocomplete-row autocomplete-selectable-element autocomplete-row-js' +
            '            {{#inTokenfield}}in-token-field{{/inTokenfield}}"> ' +
            '   <span class="element-heading">{{label}}</span>' +
            '   <span class="autocomplete-data" data-autocomplete="{{value}}" style="display: none"></span>' +
            '</div>'
    }

    var suggestions = createSuggestionsObject();

    var tokenfield = sirius.createTokenfield();
    var autocomplete = sirius.createAutocomplete();

    autocomplete.on('onSelect', function (selectedRow) {
        if (!selectedRow) {
            return;
        }

        // using .attr() instead of .data() of jQuery, because we do not want automatic type conversion of .data().
        // e.g. data-autocomplete="123" would be converted to the number 123 by .data("autocomplete").
        tokenfield.addToken(selectedRow.find('.autocomplete-data').attr('data-autocomplete'));
        tokenfield.getTokenfieldInputField().focus();

        // Trigger the change-event on the original select
        // send additional data so we know that we don't need to react to it
        $select.trigger('change', {source: 'multiselect'});
    });

    autocomplete.on('beforeRenderRow', function (row) {
        row.inTokenfield = tokenfield.hasToken(row);
    });

    tokenfield.on('onEnter', function (event) {
        if (autocomplete.getSelectedRow()) {
            // The user selected a row and pressed enter so we dont want the tokenfield to autocreate a duplicate token
            return false;
        }
        return true;
    });

    tokenfield.on('onBeforeCreateToken', function (token) {
        if (autocomplete.isPreventCreateToken()) {
            autocomplete.setPreventCreateToken(false);
            return false;
        }

        if (!suggestions.getTokenForValue(token.value)) {
            if (args.allowCustomEntries) {
                suggestions.addSuggestion(token);
                updateSelectObject();
            } else {
                if (autocomplete.getCompletionRows().length !== 1) {
                    return false;
                }
                var tokenFromRow = suggestions.getTokenForValue(autocomplete.getCompletionRows()[0].value);
                if (tokenFromRow) {
                    token.label = tokenFromRow.label;
                    token.value = tokenFromRow.value;
                } else {
                    return false;
                }
            }
        } else {
            token.label = suggestions.getTokenForValue(token.value).label;
        }

        if (tokenfield.hasToken(token)) {
            tokenfield.removeToken(token);
            return false;
        }

        if (autocomplete.getInput()) {
            autocomplete.getInput().val('');
        }

        tokenfield.getTokenfieldInputField()[0].placeholder = '';
        return true;
    });

    tokenfield.on('onCreatedToken', updateSelectObject);
    tokenfield.on('onRemovedToken', updateSelectObject);

    // Update the tokenfield if the selection in the original select is changed
    $select.on('change', function (event, data) {
        if (typeof data == 'undefined' || data.source !== 'multiselect') {
            var $selectedOptions = $select.children(':selected');
            var newTokens = [];

            $selectedOptions.each(function () {
                newTokens.push({
                    label: $(this).text(),
                    value: $(this).val()
                });
            });

            tokenfield.clearTokens();
            tokenfield.appendTokens(newTokens);
        }
    });

    tokenfield.start({
        id: args.id + '-input',
        showRemovalElement: false,
        tokenfield: {
            delimiter: '|',
            createTokensOnBlur: true,
            limit: args.maxItems
        }
    });

    if (args.maxItems === 1) {
        tokenfield.on('onCreatedToken', function (token) {
            tokenfield.getTokenfieldInputField().hide().blur();
            $('#' + args.id).find('.tokenfield').attr("tabindex", "0");
        });

        tokenfield.on('onRemovedToken', function () {
            tokenfield.getTokenfieldInputField().show();
            $('#' + args.id).find('.tokenfield').removeAttr("tabindex");
        });

        if (!args.readonly) {
            $('#' + args.id).on('click', '.tokenfield', function () {
                var oldToken = tokenfield.getTokens()[0];
                autocomplete.on('onHide', function reAddToken() {
                    if (!tokenfield.hasTokens() && oldToken) {
                        tokenfield.addToken(oldToken);
                        tokenfield.getTokenfieldInputField().val('');
                    }
                    autocomplete.off('onHide', reAddToken);
                    tokenfield.getTokenfieldInputField()[0].placeholder = args.placeholder;
                });

                if (args.keepInputOnSelect) {
                    var input = tokenfield.getInput() || "";
                    tokenfield.clearTokens();
                    // the first focus is necessary so that .val will not create a token
                    // the second focus call will trigger the autocomplete with the correct value set
                    tokenfield.getTokenfieldInputField().show().focus().val(input.trim()).focus();
                } else {
                    tokenfield.clearTokens();
                    tokenfield.getTokenfieldInputField().show().focus();
                    tokenfield.getTokenfieldInputField()[0].placeholder = args.searchKey || args.placeholder;
                }
            });

            $('#' + args.id).on('keyup', '.tokenfield', function (e) {
                if (e.target !== e.currentTarget) {
                    // only responde to key-events off the container
                    return true;
                }
                if (e.keyCode === sirius.keys.KEY_ENTER
                    || e.keyCode === sirius.keys.KEY_ARROW_DOWN
                    || e.keyCode === sirius.keys.KEY_SPACE) {
                    // when a singleselect is selected via tab, then the keys enter, space and arrow down should open
                    // the autocomplete and make the field editable, just like clicking on the field
                    $('#' + args.id).find('.tokenfield').click();
                }
            });

            $('#' + args.id).on('keydown', '.tokenfield', function (e) {
                if (e.target !== e.currentTarget) {
                    // only responde to key-events off the container
                    return true;
                }
                if (e.keyCode === sirius.keys.KEY_ARROW_DOWN || e.keyCode === sirius.keys.KEY_SPACE) {
                    // prevent scrolling the page when the field is selected via tab.
                    // this would happen by default on space or arrow down or up.
                    e.preventDefault();
                }
            });

            var $arrow = $('<span class="arrow arrow-down"/>');
            $('#' + args.id + ' .tokenfield').append($arrow);

            autocomplete.on('onShow', function () {
                $arrow.removeClass('arrow-down').addClass('arrow-up');
            });

            autocomplete.on('onHide', function () {
                $arrow.removeClass('arrow-up').addClass('arrow-down');
            });
        }
    }

    // clear tokens before appending the initial selection, because clicking back in the browser will add the old inputs
    // into the input-fields and this adds the tokens, but without the correct labels, so clear those incorrect tokens
    tokenfield.clearTokens();

    tokenfield.appendTokens(suggestions.getInitialSelection());
    updateSelectObject();

    if (!tokenfield.hasTokens()) {
        tokenfield.getTokenfieldInputField()[0].placeholder = args.placeholder;
    }

    tokenfield.on('onRemovedToken', function () {
        if (!tokenfield.hasTokens()) {
            tokenfield.getTokenfieldInputField()[0].placeholder = args.placeholder;
        }

        // Trigger the change-event on the original select
        // send additional data so we know that we don't need to react to it
        $select.trigger('change', {source: 'multiselect'});
    });

    if (!args.readonly) {
        var noMatchesToken = {label: args.noMatchesText, type: 'basic'};

        var autocompleteArgs = {
            inputField: tokenfield.getInputFieldId(),
            anchor: '#' + args.id + ' .tokenfield',
            templates: autocompleteTemplates,
            completions: {
                id: args.id + '-completion',
                height: '300px'
            },
            input: {
                noSpinner: !args.serviceUri
            }
        };

        if (args.serviceUri) {
            autocomplete.on('onSpinnerShow', function () {
                tokenfield.resizeInputField();
            });
            autocomplete.on('onSpinnerHide', function () {
                tokenfield.resizeInputField();
            });

            autocomplete.on("afterLoad", function (value, response) {
                var responseTokens = [];
                var suggestionAdded = false;
                response.completions.forEach(function (completion) {
                    responseTokens.push({
                        // label is the text displayed in the dropdown. should be what is given as "description"
                        // by the service. but use other texts as fallback.
                        label: completion.description || completion.text || completion.id,
                        value: completion.id,
                        type: 'basic'
                    });

                    if (!suggestions.getTokenForValue(completion.id)) {
                        suggestionAdded = true;
                        suggestions.addSuggestion({
                            // label is the text displayed in the tokenfield. should be what is given as "text"
                            // by the service. but use other texts as fallback.
                            label: completion.text || completion.description || completion.id,
                            value: completion.id
                        });
                    }
                });
                if (suggestionAdded) {
                    updateSelectObject();
                }
                if (responseTokens.length === 0) {
                    responseTokens.push(noMatchesToken);
                }
                return responseTokens;
            });

            autocomplete.on('onServiceError', function (request) {
                // display the dropdown with the "no search results" text
                return [noMatchesToken];
            });

            autocompleteArgs.service = {
                serviceUri: args.serviceUri,
                minSize: 0,
                delayTime: 0,
                getRequest: function (inputValue) {
                    var requestParams = {query: inputValue || '', strict: !args.allowCustomEntries};
                    if (args.type) {
                        requestParams.type = args.type;
                    }
                    return requestParams;
                }
            }
        } else {
            autocompleteArgs.localSource = {
                callback: function (query) {
                    var rows = [];
                    if (args.allowCustomEntries && query && !suggestions.getTokenForValue(query)) {
                        rows.push({
                            label: query,
                            value: query,
                            type: 'basic'
                        });
                    }
                    suggestions.forEachMatchingSuggestion(query, function (element) {
                        rows.push({
                            label: element.label,
                            value: element.value,
                            type: 'basic'
                        });
                    });
                    if (rows.length === 0) {
                        rows.push(noMatchesToken);
                    }
                    return rows;
                }
            }
        }

        autocomplete.start(autocompleteArgs);
    } else {
        // add the 'disabled' class to the tokenfield, so it looks grey and 'disabled'
        // we have to add that manually, because just 'readonly' does not look like that for the tokenfield
        // but we also don't want to disable the field, because in some browsers disabled fields are not sent when
        // posting
        $('#' + args.id + ' .tokenfield').addClass('disabled');
    }
};
