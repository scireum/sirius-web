/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.web.http.WebContext;

import javax.annotation.Nonnull;

/**
 * Detects the current {@link sirius.web.security.ScopeInfo} for a given request.
 * <p>
 * Used by {@link sirius.web.security.UserContext} to determine the scope, this request belongs to.
 *
 * @see sirius.web.security.ScopeInfo
 */
public interface ScopeDetector {

    /**
     * Detects the scope for the given request.
     *
     * @param request the request to detect the scope from
     * @return the scope this request belongs to. Use {@link sirius.web.security.ScopeInfo#DEFAULT_SCOPE} if no
     * specific scope can be detected.
     */
    @Nonnull
    ScopeInfo detectScope(@Nonnull WebContext request);
}
