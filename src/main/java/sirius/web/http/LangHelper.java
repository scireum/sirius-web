/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpRequest;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.nls.NLS;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Helper class to handle browser languages.
 */
public class LangHelper {

    private static final Pattern ACCEPT_LANGUAGE_PATTERN =
            Pattern.compile(" *([a-z]{2})(-[a-z]{2})? *(;q=([0-9.]+) *)?");

    /**
     * Returns the accepted language of the client as two-letter language code.
     *
     * @param request the HttpRequest to obtain the <tt>Accept-Language</tt> header from
     * @return the two-letter code of the accepted language of the user agent or <tt>null</tt> if no valid accept language was found
     */
    public static String fromHttpRequest(HttpRequest request) {
        double bestQ = 0;
        String currentLang = null;
        String header = request.headers().get(HttpHeaderNames.ACCEPT_LANGUAGE);
        if (Strings.isEmpty(header)) {
            return currentLang;
        }
        header = header.toLowerCase();
        for (String languageBlock : header.split(",")) {
            Matcher m = ACCEPT_LANGUAGE_PATTERN.matcher(languageBlock);
            if (m.matches()) {
                double q = Value.of(m.group(4)).asDouble(1.0d);
                String language = m.group(1);
                if (q > bestQ && NLS.isSupportedLanguage(language)) {
                    bestQ = q;
                    currentLang = language;
                }
            }
        }

        return currentLang;
    }
}
