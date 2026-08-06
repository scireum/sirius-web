/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.di.std.Register;
import sirius.web.controller.Interceptor;
import sirius.web.controller.Route;
import sirius.web.cors.AllowedOrigin;

import java.util.Collection;
import java.util.Optional;

/**
 * A test {@link Interceptor} which lets {@code CorsTest} control the {@link AllowedOrigin} strategy returned for a
 * request over a real HTTP connection.
 * <p>
 * The strategy is controlled via the static {@link #allowedOrigin} field. A {@code null} value simulates an interceptor
 * which cannot decide on a strategy (i.e. it returns an empty optional). As it is only consulted when
 * {@code http.enableCors} is disabled (see {@link TestCorsScopeDetector}), it does not interfere with requests using
 * the globally enabled CORS handling.
 */
@Register(framework = "web.test-cors")
public class TestCorsInterceptor implements Interceptor {

    /**
     * The strategy returned by {@link #determineAllowedCorsOrigin(WebContext, Collection)}, or {@code null} to signal
     * that no decision can be made.
     */
    public static volatile AllowedOrigin allowedOrigin;

    @Override
    public boolean before(WebContext webContext, Route route) {
        return false;
    }

    @Override
    public boolean beforePermissionError(String permission, WebContext webContext, Route route) {
        return false;
    }

    @Override
    public boolean shouldExecuteRoute(WebContext webContext, Route route) {
        return true;
    }

    @Override
    public Optional<AllowedOrigin> determineAllowedCorsOrigin(WebContext webContext, Collection<Route> routes) {
        return Optional.ofNullable(allowedOrigin);
    }
}
