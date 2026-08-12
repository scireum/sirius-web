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
        permits AllowedOrigin.Denied, AllowedOrigin.MatchRequest, AllowedOrigin.Specific, AllowedOrigin.Wildcard {

    /**
     * No origin is allowed at all, so no {@code Access-Control-Allow-Origin} header is set and the response stays
     * unreadable for every foreign origin.
     * <p>
     * This is <b>not</b> the same as an interceptor abstaining: an interceptor which returns an empty optional from
     * {@link Interceptor#determineAllowedCorsOrigin(WebContext, Collection)} states that it does not decide, and
     * {@link sirius.web.controller.ControllerDispatcher} then falls back to a {@link MatchRequest} - which reflects
     * the origin of the request. An interceptor that wants the response of a route to stay unreadable cross-origin
     * therefore has to say so by returning this record.
     * </p>
     * <p>
     * Note that this withholds the <i>response</i> from the calling script rather than keeping the request away from
     * the route: a simple request is still sent and still executed, and only a failing preflight stops one from
     * arriving at all. Refusing an origin is therefore no substitute for CSRF protection.
     * </p>
     */
    record Denied() implements AllowedOrigin {
    }

    /**
     * The {@code Access-Control-Allow-Origin} header should be set to the origin of the request.
     * <p>
     * Note that for security reasons the {@code Access-Control-Allow-Credentials} header will <b>not be set</b>  in
     * this case. Reflecting the request's {@code Origin} while allowing credentials behaves essentially like a wildcard
     * with allowed credentials - which is forbidden and opens an attack vector for cross-site data theft.
     * </p>
     */
    record MatchRequest() implements AllowedOrigin {
    }

    /**
     * The {@code Access-Control-Allow-Origin} header should be set to the origin of the request
     * if it is in the provided collection of allowed origins.
     * <p>
     * If it is not, the server should respond accordingly: e.g. by <i>not</i> setting the header at all, resulting in
     * a client-side CORS error.
     * </p>
     * <p>
     * Note that the {@code Access-Control-Allow-Credentials: true} header will be sent {@code true} if
     * {@code allowCredentials} is set to {@code true}.
     * By default, a value of {@code false} should be preferred, unless the specified origins explicitly need to send
     * cookies.
     * </p>
     *
     * @param allowCredentials if set to {@code true}, the {@code Access-Control-Allow-Credentials} header will be set
     *                         to {@code true}
     * @param origins          the allowed origins
     */
    record Specific(boolean allowCredentials, Collection<String> origins) implements AllowedOrigin {
        public Specific(boolean allowCredentials, String... origins) {
            this(allowCredentials, Arrays.stream(origins).collect(Collectors.toSet()));
        }
    }

    /**
     * The {@code Access-Control-Allow-Origin} header should be set to allow all origins (wildcard).
     * <p>
     * Note that the {@code Access-Control-Allow-Credentials} header will <b>not be set</b>  in this case for security
     * reasons (the combination if wildcard and allowed credentials is explicitly forbidden by the CORS standard).
     * </p>
     */
    record Wildcard() implements AllowedOrigin {
    }
}
