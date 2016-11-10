/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.di.std.Priorized;

import javax.annotation.Nonnull;

/**
 * Helpers are used by the {@link ScopeInfo} to perform certain tasks.
 * <p>
 * Note that a helper is created <b>once</b> per scope and not per user or request. Everything that is specific
 * to the user or request has the be in the request itself or the session.
 * <p>
 * Variables can be annotated using {@link HelperConfig} and will be automatically filled with the value set in the
 * scope config. HOWEVER: Note that user specific overwrites of this config value cannot be applied. These values
 * have to be fetched using {@link UserContext#getConfigValue(String)} or {@link UserInfo#getConfigValue(String)}.
 *
 * @param <H> the type of helpers created by this factory
 */
public interface HelperFactory<H> extends Priorized {

    /**
     * Returns the type of helpers produced by this factory.
     *
     * @return the class of helpers produced by this factory
     */
    @Nonnull
    Class<H> getHelperType();

    /**
     * Returns a short and descriptive name which can be used to fetch the helper in templates.
     * <p>
     * We prefer using names over class names in templates as this make refactorings way easier.
     *
     * @return the unique name used to fetch the helper in a template
     */
    @Nonnull
    String getName();

    /**
     * Creates a new helper for the given scope.
     *
     * @param scope the scope for which this helper is created
     * @return a new instance of the helper class for the given scope
     */
    @Nonnull
    H make(@Nonnull ScopeInfo scope);
}
