/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;
import sirius.kernel.commons.Value;
import sirius.kernel.commons.Values;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

/**
 * Parses and provides access to the query parameters of a request.
 *
 * @param path       Contains the effective requested URI (without the query string).
 * @param parameters Contains the parameters submitted in the query string (?param=value...).
 */
public record RequestQueryParameters(String path, Map<String, List<String>> parameters) {

    /**
     * Parses the query parameters from the given request.
     *
     * @param request the request to parse
     * @return the parsed query parameters
     */
    public static RequestQueryParameters fromRequest(HttpRequest request) {
        QueryStringDecoder decoder = new QueryStringDecoder(request.uri(), StandardCharsets.UTF_8);
        return new RequestQueryParameters(decoder.path(), decoder.parameters());
    }

    /**
     * Parses the query parameters from the given URL.
     *
     * @param url the URL to parse
     * @return the parsed query parameters
     */
    public static RequestQueryParameters fromUrl(URL url) {
        QueryStringDecoder decoder = new QueryStringDecoder(url.getQuery(), StandardCharsets.UTF_8, false);
        return new RequestQueryParameters(url.getPath(), decoder.parameters());
    }

    /**
     * Parses the query parameters from the given URI.
     *
     * @param uri the URI to parse
     * @return the parsed query parameters
     */
    public static RequestQueryParameters fromUri(URI uri) {
        QueryStringDecoder decoder = new QueryStringDecoder(uri.getQuery(), StandardCharsets.UTF_8, false);
        return new RequestQueryParameters(uri.getPath(), decoder.parameters());
    }

    /**
     * Returns a value or parameter supplied by the request.
     *
     * @param key the key used to look for the value
     * @return a Value representing the provided data.
     */
    @Nonnull
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public Value get(String key) {
        if (parameters.containsKey(key)) {
            List<String> values = getParameters(key);
            if (values.size() == 1) {
                return Value.of(values.getFirst());
            } else if (values.isEmpty()) {
                return Value.EMPTY;
            } else {
                return Value.of(values);
            }
        }
        return Value.EMPTY;
    }

    /**
     * Returns the query string parameter with the given name.
     *
     * @param key the name of the parameter to fetch
     * @return the first value or <tt>null</tt> if the parameter was not set or empty
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getParameter(String key) {
        return Values.of(getParameters(key)).at(0).getString();
    }

    /**
     * Returns all query string parameters with the given name.
     *
     * @param key the name of the parameter to fetch
     * @return all values in the query string
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public List<String> getParameters(String key) {
        if (parameters.containsKey(key)) {
            List<String> result = parameters.get(key);
            if (result != null) {
                return result;
            }
        }
        return Collections.emptyList();
    }

    /**
     * Determines if the parameter with the given name is contained in the query string.
     *
     * @param key the parameter to check for
     * @return <tt>true</tt> if the parameter is present (even if its value is <tt>null</tt>), <tt>false</tt> otherwise
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public boolean hasParameter(String key) {
        return parameters.containsKey(key);
    }

    /**
     * Returns a collection of all parameter names.
     *
     * @return a collection of all parameters in the query string
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public Collection<String> getParameterNames() {
        return new LinkedHashSet<>(parameters.keySet());
    }
}
