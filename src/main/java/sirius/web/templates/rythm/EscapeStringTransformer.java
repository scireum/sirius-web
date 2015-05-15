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

/**
 * Escapes JS strings by only replacing single quotes not double quotes (this would otherwise result in invalid
 * JavaScript (which is produced by escape("JS")).
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2015/05
 */
@Transformer("")
public class EscapeStringTransformer {
    /*
     * In contrast to escape JS / JavaScript, this only escapes a single
     * quote, but not a double quote.
     */
    public static String escapeString(String value) {
        if (Strings.isEmpty(value)) {
            return value;
        }

        return value.replace("'", "\\'");
    }

}
