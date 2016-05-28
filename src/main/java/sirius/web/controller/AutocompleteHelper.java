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

    private AutocompleteHelper() {
    }

    /**
     * Can be used with {@link sirius.kernel.nls.NLS#get(String)} to obtain a "(new)" text to be used
     * in the "description" field
     */
    public static final String NEW_ENTRY_NLS_KEY = "AutocompleteHelper.newHit";

    /**
     * Represents a suggestion or completion of a given keyword
     */
    public static class Completion {
        private String value;
        private String label;
        private String description;

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

        public Completion setLabel(String label) {
            this.label = label;
            return this;
        }

        public Completion setValue(String value) {
            this.value = value;
            return this;
        }

        public String getValue() {
            return value;
        }

        public String getLabel() {
            return label;
        }

        public String getDescription() {
            return description;
        }

        public Completion setDescription(String description) {
            this.description = description;
            return this;
        }

        private void writeTo(StructuredOutput out) {
            out.beginObject("completion");
            {
                out.property("id", value);
                out.property("text", label == null ? "" : label);
                if (Strings.isFilled(description)) {
                    out.property("description", description);
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
