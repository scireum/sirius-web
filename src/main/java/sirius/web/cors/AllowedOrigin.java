/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.cors;

import sirius.web.controller.Interceptor;
import sirius.web.http.WebContext;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * Defines the different types of allowed origins an {@code Access-Control-Allow-Origin} HTTP header may hold.
 * <p>
 * Subclasses of this interface are returned by
 * {@link Interceptor#determineAllowedCorsOrigin(WebContext, Collection)} to define the allowed origins for a
 * given route.
 * </p>
 */
public sealed interface AllowedOrigin
        permits AllowedOrigin.MatchRequest, AllowedOrigin.Specific, AllowedOrigin.Wildcard {

    /**
     * The {@code Access-Control-Allow-Origin} header should be set to the origin of the request.
     */
    record MatchRequest() implements AllowedOrigin {
    }

    /**
     * The {@code Access-Control-Allow-Origin} header should be set to the origin of the request
     * <b>if it is in the provided collection of allowed origins</b>.
     * <p>
     * If it is not, the server should respond accordingly: e.g. by <i>not</i> setting the header at all, resulting in
     * a client-side CORS error.
     * </p>
     *
     * @param origins the allowed origins
     */
    record Specific(Collection<String> origins) implements AllowedOrigin {
        public Specific(String... origins) {
            this(Arrays.stream(origins).collect(Collectors.toSet()));
        }
    }

    /**
     * The {@code Access-Control-Allow-Origin} header should be set to allow all origins (wildcard).
     */
    record Wildcard() implements AllowedOrigin {
    }
}
