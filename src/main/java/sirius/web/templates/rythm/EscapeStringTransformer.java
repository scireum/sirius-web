/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import org.rythmengine.extension.Transformer;
import sirius.kernel.commons.Strings;

import javax.annotation.Nullable;

/**
 * Escapes JS strings by only replacing single quotes not double quotes (this would otherwise result in invalid
 * JavaScript (which is produced by escape("JS")).
 */
@Transformer("")
public class EscapeStringTransformer {

    private EscapeStringTransformer() {
    }

    /**
     * In contrast to escape JS / JavaScript, this only escapes a single
     * quote, but not a double quote.
     *
     * @param value the string to escape
     * @return the escaped string
     */
    @Nullable
    public static String escapeString(@Nullable String value) {
        if (Strings.isEmpty(value)) {
            return value;
        }

        return value.replace("'", "\\'");
    }
}
