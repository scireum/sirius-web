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

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Supplier;

/**
 * Resolves the effective {@code Access-Control-Allow-Origin} for a request and applies it to the response.
 * <p>
 * An {@link AllowedOrigin} (as determined by the {@link sirius.web.controller.Interceptor interceptors}) describes the
 * <i>strategy</i> for the header. This helper turns that strategy into a concrete origin for the current request,
 * stores it in the request scoped {@link CorsContext}, and later writes it to the response as the actual CORS header.
 * </p>
 */
@Register(classes = CorsAllowOriginHelper.class)
public class CorsAllowOriginHelper {

    /**
     * Stores the given strategy in the request's {@link CorsContext} and, if it resolves to a concrete origin for the
     * current request, stores that resolved origin as well.
     * <p>
     * If automatic CORS handling is disabled for the request's scope (i.e. {@link WebContext#isCorsAllowAll()} returns
     * {@code false}), this method does nothing: neither the strategy nor a resolved origin is stored, so no CORS
     * headers are emitted for the request. {@code corsAllowAll} therefore acts as a master switch for the whole CORS
     * handling.
     * </p>
     * <p>
     * Otherwise the strategy is recorded (so that e.g. the {@code Vary} header can be derived later), while the concrete
     * origin is only stored if the strategy resolves to one. If the strategy resolves to no origin (e.g. an
     * {@link AllowedOrigin.Specific} whose collection does not contain the request's origin), no resolved origin is
     * stored and no {@code Access-Control-Allow-Origin} header will be emitted.
     * </p>
     * <p>
     * <b>Note on performance:</b> The supplier is only invoked when CORS handling is enabled, so an expensive
     * strategy computation (e.g. consulting interceptors) is deferred and skipped entirely when {@code corsAllowAll}
     * is disabled. Statically known strategies may be passed directly via the overloaded variant of this method.
     * </p>
     *
     * @param webContext             the current request
     * @param allowedOriginSupplier  supplies the allowed origin strategy to resolve and store
     */
    public void tryResolveAndStoreOrigin(WebContext webContext, Supplier<AllowedOrigin> allowedOriginSupplier) {
        // If `corsAllowAll` is disabled, we explicitly do not want to handle anything CORS.
        if (!webContext.isCorsAllowAll()) {
            return;
        }

        var corsContext = CorsContext.get();
        var origin = allowedOriginSupplier.get();

        corsContext.setConfiguredOrigin(origin);
        tryResolveOrigin(webContext, origin).ifPresent(corsContext::setResolvedOrigin);
    }

    /**
     * Convenience overload of {@link #tryResolveAndStoreOrigin(WebContext, Supplier)} for a statically known strategy.
     * <p>
     * <b>Note on performance:</b> The origin is only considered when CORS handling is enabled, so an expensive
     * strategy computation (e.g. consulting interceptors) should be deferred via
     * {@link #tryResolveAndStoreOrigin(WebContext, Supplier)} so it can be skipped entirely when {@code corsAllowAll}
     * is disabled!
     * </p>
     *
     * @param webContext            the current request
     * @param origin     the allowed origin strategy to resolve and store
     */
    public void tryResolveAndStoreOrigin(WebContext webContext, AllowedOrigin origin) {
        tryResolveAndStoreOrigin(webContext, () -> origin);
    }

    /**
     * Returns the strategy configured for the current request via {@link #tryResolveAndStoreOrigin(WebContext,
     * AllowedOrigin)}, if any.
     *
     * @return the configured {@link AllowedOrigin} strategy, or an empty optional if none has been stored for the
     * current request
     */
    public Optional<AllowedOrigin> getConfiguredOrigin() {
        return CorsContext.get().getConfiguredOrigin();
    }

    /**
     * Applies the origin resolved for the current request to the given {@link Response} as an
     * {@code Access-Control-Allow-Origin} header.
     * <p>
     * If no origin has been resolved for the current request, no header is set.
     * </p>
     *
     * @param response the response to set the header on
     * @return {@code true} if a resolved origin was applied, {@code false} if none had been resolved for the request
     */
    public boolean applyResolvedOriginHeader(Response response) {
        return applyResolvedOriginHeader(response::setHeader);
    }

    /**
     * Applies the origin resolved for the current request to the given {@link HttpResponse} as an
     * {@code Access-Control-Allow-Origin} header.
     * <p>
     * If no origin has been resolved for the current request, no header is set.
     * </p>
     *
     * @param response the response to set the header on
     * @return {@code true} if a resolved origin was applied, {@code false} if none had been resolved for the request
     */
    public boolean applyResolvedOriginHeader(HttpResponse response) {
        return applyResolvedOriginHeader(response.headers()::set);
    }

    private boolean applyResolvedOriginHeader(BiFunction<CharSequence, CharSequence, ?> setHeader) {
        Optional<String> resolvedOrigin = CorsContext.get().getResolvedOrigin();
        resolvedOrigin.ifPresent(origin -> {
            setHeader.apply(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN, origin);
        });
        return resolvedOrigin.isPresent();
    }

    private Optional<String> tryResolveOrigin(WebContext webContext, AllowedOrigin origin) {
        @Nullable String requestOrigin = webContext.getHeader(HttpHeaderNames.ORIGIN);

        return switch (origin) {
            // Using `Optional.ofNullable()` implicitly handles cases where no `Origin` header is available (e.g.
            // due to the browser not sending it for same-origin requests). Returning an empty optional results in no
            // header being set, which is the expected behavior in such a case.
            case AllowedOrigin.MatchRequest _ -> Optional.ofNullable(requestOrigin);
            case AllowedOrigin.Specific specific -> resolveMultipleAllowedOrigins(specific.origins(), requestOrigin);
            case AllowedOrigin.Wildcard _ -> Optional.of("*");
        };
    }

    private Optional<String> resolveMultipleAllowedOrigins(Collection<String> origins, @Nullable String requestOrigin) {
        if (requestOrigin == null) {
            return Optional.empty();
        }

        // If the request origin is not in the list of allowed origins, we return an empty Optional in order to
        // reject the request (an empty optional results in no header being set).
        return origins.contains(requestOrigin) ? Optional.of(requestOrigin) : Optional.empty();
    }
}
