/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.typesafe.config.ConfigFactory;
import sirius.kernel.di.std.Register;
import sirius.web.security.ScopeDetector;
import sirius.web.security.ScopeInfo;

import javax.annotation.Nonnull;
import java.util.Optional;

/**
 * A test {@link ScopeDetector} which allows {@code CorsTest} to disable the automatic CORS handling
 * ({@code http.enableCors}) for an individual request over a real HTTP connection.
 * <p>
 * Since the global test configuration enables {@code http.enableCors}, the {@link sirius.web.controller.Interceptor}
 * based CORS origin resolution would never be reached. Requests carrying the {@link #HEADER_DISABLE_CORS_ALL} header
 * are therefore bound to a scope which overrides the setting to {@code false}. All other requests use the
 * {@link ScopeInfo#DEFAULT_SCOPE} and hence keep the globally enabled handling.
 */
@Register(framework = "web.test-cors")
public class TestCorsScopeDetector implements ScopeDetector {

    /**
     * Requests carrying this header are bound to a scope which disables automatic CORS handling.
     */
    public static final String HEADER_DISABLE_CORS_ALL = "X-Test-Disable-Cors-All";

    private static final ScopeInfo CORS_RESTRICTED_SCOPE = new ScopeInfo("cors-restricted",
                                                                         ScopeInfo.DEFAULT_SCOPE.getScopeType(),
                                                                         "cors-restricted",
                                                                         null,
                                                                         scope -> ConfigFactory.parseString(
                                                                                 "http.enableCors = false"),
                                                                         null);

    @Nonnull
    @Override
    public ScopeInfo detectScope(@Nonnull WebContext request) {
        if (request.getHeader(HEADER_DISABLE_CORS_ALL) != null) {
            return CORS_RESTRICTED_SCOPE;
        }
        return ScopeInfo.DEFAULT_SCOPE;
    }

    @Nonnull
    @Override
    public ScopeInfo findScopeByName(@Nonnull String scopeName) {
        return ScopeInfo.DEFAULT_SCOPE;
    }

    @Nonnull
    @Override
    public Optional<ScopeInfo> findScopeById(@Nonnull String scopeId) {
        return Optional.empty();
    }
}
