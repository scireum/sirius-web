/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.web.security.UserContext;

import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Helper class to handle browser languages.
 */
public class LangHelper {

    private static final Pattern ACCEPT_LANGUAGE_PATTERN =
            Pattern.compile(" *([a-z]{2})(-[a-z]{2})? *(;q=([0-9.]+) *)?");

    private LangHelper() {
    }

    /**
     * Returns the accepted language of the client as two-letter language code.
     *
     * @param acceptLanguage the <tt>Accept-Language</tt> e.g. from a HTTP-Request
     * @return the two-letter code of the accepted language of the user agent or <tt>{@link Optional#empty()}</tt> if no valid accept language was found
     */
    public static Optional<String> from(String acceptLanguage) {
        if (Strings.isEmpty(acceptLanguage)) {
            return Optional.empty();
        }

        double bestQ = 0;
        String currentLang = null;

        for (String languageBlock : acceptLanguage.toLowerCase().split(",")) {
            Matcher m = ACCEPT_LANGUAGE_PATTERN.matcher(languageBlock);
            if (m.matches()) {
                double q = Value.of(m.group(4)).asDouble(1.0d);
                String language = m.group(1);
                if (q > bestQ && UserContext.getCurrentScope().isSupportedLanguage(language)) {
                    bestQ = q;
                    currentLang = language;
                }
            }
        }

        return Optional.ofNullable(currentLang);
    }
}
