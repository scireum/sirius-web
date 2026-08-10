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
 * {@link CorsAllowOriginResolver} determines the effective {@code Access-Control-Allow-Origin} early during dispatching
 * and stores it here, so that it can later be applied when the response is created - without having to thread it
 * through the call chain or resort to string keyed request attributes.
 * <p>
 * As several dispatchers/interceptors may be invoked for a single request, an origin may only ever be resolved once
 * per request. Once this context is marked as {@linkplain #markFinalized() finalized}, {@link #setConfiguredOrigin(AllowedOrigin)}
 * and {@link #setResolvedOrigin(String)} refuse any further modification. This guarantees that the first strategy to
 * resolve an origin wins and can never be weakened or overridden by a later invocation.
 */
public class CorsContext implements SubContext {

    private boolean isFinalized = false;

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
     * Marks this context as finalized, permanently locking in the currently stored origin.
     * <p>
     * Once finalized, {@link #setConfiguredOrigin(AllowedOrigin)} and {@link #setResolvedOrigin(String)} will throw
     * an {@link IllegalStateException} instead of overriding the previously stored values. This is used to ensure
     * that an origin can only be resolved once per request, so that later dispatchers/interceptors cannot weaken or
     * override a decision that was already made.
     */
    public void markFinalized() {
        isFinalized = true;
    }

    /**
     * Determines whether this context has already been finalized.
     *
     * @return {@code true} if the origin for this request has already been resolved and finalized, {@code false}
     * otherwise
     */
    public boolean isFinalized() {
        return isFinalized;
    }

    /**
     * Stores the CORS origin strategy configured for the current request.
     *
     * @param configuredOrigin the configured origin strategy to store
     * @throws IllegalStateException if the context has already been finalized
     */
    public void setConfiguredOrigin(@Nullable AllowedOrigin configuredOrigin) {
        if (isFinalized) {
            throw new IllegalStateException("Cannot modify finalized CORS context");
        }
        this.configuredOrigin = configuredOrigin;
    }

    /**
     * Stores the origin resolved for the current request.
     *
     * @param resolvedOrigin the resolved origin to store
     * @throws IllegalStateException if the context has already been finalized
     */
    public void setResolvedOrigin(@Nullable String resolvedOrigin) {
        if (isFinalized) {
            throw new IllegalStateException("Cannot modify finalized CORS context");
        }
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
