/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

/**
 * Helpers are used by the {@link ScopeInfo} to perform certain tasks.
 * <p>
 * Note that a helper is created <b>once</b> per scope and not per user or request. Everything that is specific
 * to the user or request has the be in the request itself or the session.
 *
 * @param <H> the type of helpers created by this factory
 */
public interface HelperFactory<H> {

    /**
     * Returns the type of helpers produced by this factory.
     *
     * @return the class of helpers produced by this factory
     */
    Class<H> getHelperType();

    /**
     * Creates a new helper for the given scope.
     *
     * @param scope the scope for which this helper is created
     * @return a new instance of the helper class for the given scope
     */
    H make(ScopeInfo scope);
}
