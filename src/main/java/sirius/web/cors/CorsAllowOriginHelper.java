/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.cors;

import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpResponse;
import sirius.kernel.di.std.Register;
import sirius.web.http.Response;
import sirius.web.http.WebContext;

import java.util.Collection;
import java.util.Optional;
import java.util.function.BiFunction;

/**
 * Resolves the effective {@code Access-Control-Allow-Origin} for a request and applies it to the response.
 * <p>
 * An {@link AllowedOrigin} (as determined by the {@link sirius.web.controller.Interceptor interceptors}) describes the
 * <i>strategy</i> for the header. This helper turns that strategy into a concrete origin for the current request,
 * stores it in the {@link WebContext}, and later writes it to the response as the actual CORS header.
 * </p>
 */
@Register(classes = CorsAllowOriginHelper.class)
public class CorsAllowOriginHelper {

    private static final String ATTRIBUTE_CORS_ORIGIN = "sirius_corsAllowOrigin";

    /**
     * Resolves the concrete origin for the given strategy and, if one is applicable, stores it in the request.
     * <p>
     * If the strategy resolves to no origin (e.g. an {@link AllowedOrigin.Specific} whose collection does not contain
     * the request's origin), nothing is stored and no CORS header will be emitted.
     * </p>
     *
     * @param webContext the current request
     * @param origin     the allowed origin strategy to resolve
     */
    public void tryResolveOriginAndStoreInWebContext(WebContext webContext, AllowedOrigin origin) {
        tryResolveOrigin(webContext, origin).ifPresent(resolvedOrigin -> {
            storeOriginInWebContext(webContext, resolvedOrigin);
        });
    }

    /**
     * Applies the previously resolved CORS origin to the given {@link Response} as an
     * {@code Access-Control-Allow-Origin} header.
     * <p>
     * If no origin has been resolved and stored for the current request, no header is set.
     * </p>
     *
     * @param webContext the current request
     * @param response   the response to set the header on
     */
    public void applyHeaderFromWebContext(WebContext webContext, Response response) {
        applyHeaderFromWebContext(webContext, response::setHeader);
    }

    /**
     * Applies the previously resolved CORS origin to the given {@link HttpResponse} as an
     * {@code Access-Control-Allow-Origin} header.
     * <p>
     * If no origin has been resolved and stored for the current request, no header is set.
     * </p>
     *
     * @param webContext the current request
     * @param response   the response to set the header on
     */
    public void applyHeaderFromWebContext(WebContext webContext, HttpResponse response) {
        applyHeaderFromWebContext(webContext, response.headers()::set);
    }

    private void applyHeaderFromWebContext(WebContext webContext, BiFunction<CharSequence, CharSequence, ?> setHeader) {
        getOriginFromWebContext(webContext).ifPresent(origin -> {
            setHeader.apply(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN, origin);
        });
    }

    private Optional<String> tryResolveOrigin(WebContext webContext, AllowedOrigin origin) {
        String requestOrigin = webContext.getHeader(HttpHeaderNames.ORIGIN);
        return switch (origin) {
            case AllowedOrigin.MatchRequest _ -> Optional.ofNullable(requestOrigin);
            case AllowedOrigin.Specific specific -> resolveMultipleAllowedOrigins(specific.origins(), requestOrigin);
            case AllowedOrigin.Wildcard _ -> Optional.of("*");
        };
    }

    private Optional<String> resolveMultipleAllowedOrigins(Collection<String> origins, String requestOrigin) {
        return origins.contains(requestOrigin) ? Optional.of(requestOrigin) : Optional.empty();
    }

    private void storeOriginInWebContext(WebContext webContext, String origin) {
        webContext.setAttribute(ATTRIBUTE_CORS_ORIGIN, origin);
    }

    private Optional<String> getOriginFromWebContext(WebContext webContext) {
        return webContext.safeGet(ATTRIBUTE_CORS_ORIGIN).asOptionalString();
    }
}
