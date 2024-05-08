/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.QueryStringDecoder;
import sirius.kernel.commons.Value;
import sirius.kernel.commons.Values;

import javax.annotation.Nonnull;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Wraps a {@link QueryStringDecoder} to provide some additional boilerplate methods.
 */
public class QueryString {

    private final QueryStringDecoder decoder;

    /**
     * Parses the given URI string (consisting of path and query parameters) using a {@link QueryStringDecoder}.
     *
     * @param uri the URI string to parse
     */
    public QueryString(@Nonnull String uri) {
        decoder = new QueryStringDecoder(uri, StandardCharsets.UTF_8);
    }

    /**
     * Parses the given URI using a {@link QueryStringDecoder}.
     *
     * @param uri the URI to parse
     */
    public QueryString(URI uri) {
        decoder = new QueryStringDecoder(uri);
    }

    /**
     * Returns the path of the URI.
     *
     * @return the path part of the given uri
     */
    @Nonnull
    public String path() {
        return decoder.path();
    }

    /**
     * Returns the first value of the given parameter wrapped as value.
     *
     * @param key the name of the parameter to fetch
     * @return the first value which was part of the query string in the given URI or an empty value if the parameter
     * isn't present
     */
    @Nonnull
    public Value get(@Nonnull String key) {
        List<String> values = getParameters(key);
        if (values.isEmpty()) {
            // No entries
            return Value.EMPTY;
        }
        if (values.size() == 1) {
            // Exactly one entry
            return Value.of(values.getFirst());
        }
        // Many entries
        return Value.of(values);
    }

    /**
     * Returns the query string parameter with the given name.
     *
     * @param key the name of the parameter to fetch
     * @return the first value or <tt>null</tt> if the parameter was not set or empty
     */
    public String getParameter(String key) {
        return Values.of(getParameters(key)).at(0).getString();
    }

    /**
     * Determines if the given parameter is present in the query string.
     * <p>
     * This can be used to distinguish a missing parameter from <tt>param=</tt>.
     *
     * @param key the name of the parameter to check
     * @return <tt>true</tt> if the parameter is present, <tt>false</tt> otherwise
     */
    public boolean hasParameter(@Nonnull String key) {
        return decoder.parameters().containsKey(key);
    }

    /**
     * Returns all values present for the given parameter.
     *
     * @param key the name of the parameter to fetch
     * @return a list of all values for the given parameter or an empty list if no value is present
     */
    @Nonnull
    public List<String> getParameters(@Nonnull String key) {
        List<String> values = decoder.parameters().get(key);
        if (values == null) {
            return Collections.emptyList();
        }
        return values;
    }

    /**
     * Returns a collection of all parameter names.
     *
     * @return a collection of all parameters in the query string
     */
    public Set<String> getParameterNames() {
        return new LinkedHashSet<>(decoder.parameters().keySet());
    }

    /**
     * Provides access to the underlying decoder.
     *
     * @return the decoder which was used to parse the query string
     */
    @Nonnull
    public QueryStringDecoder getDecoder() {
        return decoder;
    }
}
