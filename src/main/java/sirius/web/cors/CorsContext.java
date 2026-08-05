/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.cors;

import sirius.kernel.async.CallContext;
import sirius.kernel.async.SubContext;

import javax.annotation.Nullable;
import java.util.Optional;

/**
 * Carries the CORS origin resolved for the current request as a {@link SubContext}.
 * <p>
 * {@link CorsAllowOriginHelper} determines the effective {@code Access-Control-Allow-Origin} early during dispatching
 * and stores it here, so that it can later be applied when the response is created - without having to thread it
 * through the call chain or resort to string keyed request attributes.
 */
public class CorsContext implements SubContext {

    @Nullable
    private AllowedOrigin configuredOrigin;

    @Nullable
    private String resolvedOrigin;

    /**
     * Returns the {@link CorsContext} of the current request.
     *
     * @return the CORS context associated with the current {@link CallContext}
     */
    public static CorsContext get() {
        return CallContext.getCurrent().getOrCreateSubContext(CorsContext.class);
    }

    /**
     * Stores the CORS origin strategy configured for the current request.
     *
     * @param configuredOrigin the configured origin strategy to store
     */
    public void setConfiguredOrigin(@Nullable AllowedOrigin configuredOrigin) {
        this.configuredOrigin = configuredOrigin;
    }

    /**
     * Stores the origin resolved for the current request.
     *
     * @param resolvedOrigin the resolved origin to store
     */
    public void setResolvedOrigin(@Nullable String resolvedOrigin) {
        this.resolvedOrigin = resolvedOrigin;
    }

    /**
     * Returns the CORS origin strategy configured for the current request, if any.
     *
     * @return the configured origin strategy, or an empty optional if none has been configured yet
     */
    public Optional<AllowedOrigin> getConfiguredOrigin() {
        return Optional.ofNullable(configuredOrigin);
    }

    /**
     * Returns the origin resolved for the current request, if any.
     *
     * @return the resolved origin, or an empty optional if none has been resolved yet
     */
    public Optional<String> getResolvedOrigin() {
        return Optional.ofNullable(resolvedOrigin);
    }

    @Override
    public SubContext fork() {
        // The resolved origin is valid for the whole request, hence it may be shared with forked sub tasks.
        return this;
    }

    @Override
    public void detach() {
        // Nothing to detach.
    }
}
