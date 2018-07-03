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

import javax.annotation.Nullable;
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

    private AutocompleteHelper() {
    }

    /**
     * Represents a suggestion or completion of a given keyword
     */
    public static class Completion {
        private String value;
        private String label;
        private String description;
        private boolean disabled = false;

        /**
         * Generates a new suggestion
         *
         * @param value       the effective value to fill into the field
         * @param label       the text to display to the user
         * @param description the text shown in the autocomplete-dropdown
         */
        public Completion(String value, String label, @Nullable String description) {
            this.value = value;
            this.label = label;
            this.description = description;
        }

        /**
         * Sets the label for the suggestion
         *
         * @param label the label shown to the user
         * @return the completion itself for fluent method calls
         */
        public Completion setLabel(String label) {
            this.label = label;
            return this;
        }

        /**
         * Sets the value for the suggestion
         *
         * @param value the value used in the entry field
         * @return the completion itself for fluent method calls
         */
        public Completion setValue(String value) {
            this.value = value;
            return this;
        }

        /**
         * Returns the effective value of the suggestion
         *
         * @return the effective value
         */
        public String getValue() {
            return value;
        }

        /**
         * Returns the label shown to the user
         *
         * @return the label of the suggestion
         */
        public String getLabel() {
            return label;
        }

        /**
         * Returns the description of the suggestion
         *
         * @return the description to show to the user
         */
        public String getDescription() {
            return description;
        }

        /**
         * Sets the description for the suggestion
         *
         * @param description the description to show to the user
         * @return the completion itself for fluent method calls
         */
        public Completion setDescription(String description) {
            this.description = description;
            return this;
        }

        /**
         * Sets if the suggestion should be disabled
         * <p>
         * Uses standard select2 functionality to have entries in the select, that can't be selected.
         *
         * @param disabled <tt>true</tt> if the suggestion is disabled, <tt>false</tt> otherwise
         */
        public void setDisabled(boolean disabled) {
            this.disabled = disabled;
        }

        private void writeTo(StructuredOutput out) {
            out.beginObject("completion");
            {
                out.property("id", value);
                out.property("text", label == null ? "" : label);
                if (Strings.isFilled(description)) {
                    out.property("description", description);
                }
                if (disabled) {
                    out.property("disabled", true);
                }
            }
            out.endObject();
        }
    }

    /**
     * Called to generate completions for a given query.
     */
    public interface ItemSearch {
        void search(String query, Consumer<Completion> result);
    }

    /**
     * Handles the given request and generates the appropriate JSON as expected by the select2 binding.
     *
     * @param ctx    the request to handle
     * @param search the handler to generate suggestions
     */
    public static void handle(WebContext ctx, ItemSearch search) {
        StructuredOutput out = ctx.respondWith().json();
        out.beginResult();
        out.beginArray("completions");
        search.search(ctx.get("query").asString(), c -> {
            if (Strings.isFilled(c.getValue())) {
                c.writeTo(out);
            }
        });
        out.endArray();
        out.endResult();
    }
}
