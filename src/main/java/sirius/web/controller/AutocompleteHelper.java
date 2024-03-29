/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.commons.Strings;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.http.WebContext;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

/**
 * Used to generate a response expected by autocomplete-able input fields.
 * <p>
 * Generates a response expected by our binding in the autocomplete templates.
 */
public class AutocompleteHelper {

    /**
     * Can be used with {@link sirius.kernel.nls.NLS#get(String)} to obtain a "(new)" text to be used
     * in the "description" field
     */
    public static final String NEW_ENTRY_NLS_KEY = "AutocompleteHelper.newHit";

    /**
     * Specifies the default number of suggestions to supply.
     */
    public static final int DEFAULT_LIMIT = 25;

    private AutocompleteHelper() {
    }

    /**
     * Represents a suggestion or completion of a given keyword
     */
    public static class Completion {
        private final String value;
        private String fieldLabel;
        private String completionLabel;
        private String completionDescription;
        private boolean disabled = false;

        /**
         * Generates a new suggestion
         *
         * @param value the effective value to fill into the field
         */
        protected Completion(String value) {
            this.value = value;
            this.fieldLabel = value;
        }

        /**
         * Specifies the label to be shown in the field itself.
         *
         * @param fieldLabel the label shown to the user
         * @return the completion itself for fluent method calls
         */
        public Completion withFieldLabel(String fieldLabel) {
            this.fieldLabel = fieldLabel;
            return this;
        }

        /**
         * Specifies the label to be shown within the completion of the autocomplete.
         *
         * @param completionLabel the label shown to the user
         * @return the completion itself for fluent method calls
         */
        public Completion withCompletionLabel(String completionLabel) {
            this.completionLabel = completionLabel;
            return this;
        }

        /**
         * Sets the description for the suggestion
         *
         * @param completionDescription the description to show to the user
         * @return the completion itself for fluent method calls
         */
        public Completion withCompletionDescription(String completionDescription) {
            this.completionDescription = completionDescription;
            return this;
        }

        /**
         * Marks the suggestion as disabled.
         *
         * @return the completion itself for fluent method calls
         */
        public Completion markDisabled() {
            this.disabled = true;
            return this;
        }

        private void writeTo(StructuredOutput output) {
            output.beginObject("completion");
            {
                output.property("value", value);
                output.property("fieldLabel", fieldLabel);
                output.property("completionLabel", Strings.isFilled(completionLabel) ? completionLabel : fieldLabel);
                output.property("completionDescription", completionDescription);

                // LEGACY SUPPORT....
                output.property("id", value);
                output.property("text", fieldLabel == null ? "" : fieldLabel);
                output.property("description", Strings.isFilled(completionLabel) ? completionLabel : fieldLabel);
                // END OF LEGACY SUPPORT

                if (disabled) {
                    output.property("disabled", true);
                }
            }
            output.endObject();
        }
    }

    /**
     * Called to generate completions for a given query.
     */
    public interface ItemSearch {
        /**
         * Invoked to generate completions.
         *
         * @param query  the input provided by the user
         * @param result the consumer to be supplied with completions for the autocomplete
         */
        void search(String query, Consumer<Completion> result);
    }

    /**
     * Creates a new completion for the given code.
     * <p>
     * Note that most probably at least {@link Completion#withFieldLabel(String)} should be used to provide
     * a proper label for the completion.
     *
     * @param code the code to use as value.
     * @return the completion for the given code
     */
    public static Completion suggest(String code) {
        return new Completion(code);
    }

    /**
     * Handles the given request and generates the appropriate JSON as expected by the autocomplete in JavaScript.
     *
     * @param webContext the request to handle
     * @param search     the handler to generate suggestions
     */
    public static void handle(WebContext webContext, ItemSearch search) {
        StructuredOutput output = webContext.respondWith().json();
        output.beginResult();
        output.beginArray("completions");
        search.search(webContext.get("query").asString(), c -> {
            if (Strings.isFilled(c.value)) {
                c.writeTo(output);
            }
        });
        output.endArray();
        output.endResult();
    }

    /**
     * Handles the given request and generates the appropriate JSON as expected by the autocomplete in JavaScript.
     * Also adds a "hasMore" entry with the given text if the given limit is reached.
     *
     * @param webContext the request to handle
     * @param search     the handler to generate suggestions
     * @param limit      the maximum number of suggestions to generate
     * @param hint       the hint to show for the "hasMore" entry
     */
    public static void handleWithMore(WebContext webContext, ItemSearch search, int limit, String hint) {
        AtomicInteger counter = new AtomicInteger();
        StructuredOutput output = webContext.respondWith().json();
        output.beginResult();
        output.beginArray("completions");
        search.search(webContext.get("query").asString(), completion -> {
            if (counter.get() < limit) {
                if (Strings.isFilled(completion.value)) {
                    completion.writeTo(output);
                    counter.incrementAndGet();
                }
            } else if (counter.get() == limit) {
                new Completion("hasMore").markDisabled().withFieldLabel(hint).writeTo(output);
                counter.incrementAndGet();
            }
        });
        output.endArray();
        output.endResult();
    }

    /**
     * Handles the given request and generates the appropriate JSON as expected by the autocomplete in JavaScript.
     * Also adds a "hasMore" entry with "..." if the given limit is reached.
     *
     * @param webContext the request to handle
     * @param search     the handler to generate suggestions
     * @param limit      the maximum number of suggestions to generate
     */
    public static void handleWithMore(WebContext webContext, ItemSearch search, int limit) {
        handleWithMore(webContext, search, limit, "...");
    }

    /**
     * Handles the given request and generates the appropriate JSON as expected by the autocomplete in JavaScript.
     * Also adds a "hasMore" entry with the given text if the DEFAULT_LIMIT limit is reached.
     *
     * @param webContext the request to handle
     * @param search     the handler to generate suggestions
     * @param hint       the hint to show for the "hasMore" entry
     */
    public static void handleWithMore(WebContext webContext, ItemSearch search, String hint) {
        handleWithMore(webContext, search, DEFAULT_LIMIT, hint);
    }

    /**
     * Handles the given request and generates the appropriate JSON as expected by the autocomplete in JavaScript.
     * Also adds a "hasMore" entry with "..." if the DEFAULT_LIMIT is reached.
     *
     * @param webContext the request to handle
     * @param search     the handler to generate suggestions
     */
    public static void handleWithMore(WebContext webContext, ItemSearch search) {
        handleWithMore(webContext, search, DEFAULT_LIMIT, "...");
    }
}
