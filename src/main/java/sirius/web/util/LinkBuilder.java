/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Urls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Helper class to build links containing query strings.
 */
public class LinkBuilder {

    private final StringBuilder stringBuilder;
    private boolean queryStringStarted;

    /**
     * Creates a new builder based on the initial URL.
     * <p>
     * This can be either a fully qualified URL like <tt><a href="https://somehost.com">...</a></tt> or an URI like <tt>/some/uri</tt>
     * or even one with a query string already like <tt>/test?some=parameter</tt>.
     *
     * @param uriOrLink the base url to extend. This value can be empty but must not be <tt>null</tt>
     */
    public LinkBuilder(@Nonnull String uriOrLink) {
        this.stringBuilder = new StringBuilder(uriOrLink);
        this.queryStringStarted = uriOrLink.contains("?");
    }

    /**
     * Adds a name/value pair to the link as given.
     * <p>
     * This will <b>NOT</b> url encode the given value but it will properly extend the URL by ? or &amp; to append
     * the given parameter to the query string.
     *
     * @param name  the name of the parameter to add
     * @param value the value to add (without any further processing)
     * @return the builder itself for fluent method calls
     */
    public LinkBuilder appendRaw(@Nonnull String name, @Nullable String value) {
        if (queryStringStarted) {
            stringBuilder.append("&");
        } else {
            stringBuilder.append("?");
            queryStringStarted = true;
        }

        stringBuilder.append(name);
        stringBuilder.append("=");
        stringBuilder.append(value);

        return this;
    }

    /**
     * Adds a name/value pair to the link as given.
     * <p>
     * This the given value will be url encoded using {@link Urls#encode(String)}. A <tt>null</tt>
     * value will be replaced with "". If a non-string object is given, <tt>toString()</tt> will be called upon it.
     *
     * @param name  the name of the parameter to add
     * @param value the value to add (without any further processing)
     * @return the builder itself for fluent method calls
     */
    @SuppressWarnings("squid:S2259")
    @Explain("Accessing value is null-safe due to Strings.isEmpty check")
    public LinkBuilder append(@Nonnull String name, @Nullable Object value) {
        if (Strings.isEmpty(value)) {
            return appendRaw(name, "");
        } else {
            return appendRaw(name, Urls.encode(value.toString()));
        }
    }

    /**
     * Adds a name/value pair to the link if the <tt>value</tt> is {@link Strings#isFilled(Object) filled}.
     *
     * <p>
     * Just like {@link #append(String, Object)} this will escape the given value.
     *
     * @param name  the name of the parameter to add
     * @param value the value to add (without any further processing)
     * @return the builder itself for fluent method calls
     */
    @SuppressWarnings("squid:S2259")
    @Explain("Accessing value is null-safe due to Strings.isFilled check")
    public LinkBuilder appendIfFilled(@Nonnull String name, @Nullable Object value) {
        if (Strings.isFilled(value)) {
            appendRaw(name, Urls.encode(value.toString()));
        }

        return this;
    }

    @Override
    public String toString() {
        return stringBuilder.toString();
    }
}
