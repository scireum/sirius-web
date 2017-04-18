/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.di.std.Register;
import sirius.kernel.settings.Extension;

import javax.annotation.Nonnull;

/**
 * Creates a new {@link UserManager} for a given {@link ScopeInfo}.
 * <p>
 * Implementations of this class must wear an {@link sirius.kernel.di.std.Register} annotation with a {@link
 * Register#name()}. This name is referenced in the system config (<tt>security.scope.[scopeType].manager</tt> to
 * select which manager to use.
 */
public interface UserManagerFactory {

    /**
     * Creates a new user manager for the given scope and config settings (the block in
     * <tt>security.scope.[scopeType]</tt>).
     *
     * @param scope  the scope for which the user manager is to be created
     * @param config the config section from the system config
     * @return the newly created user manager
     */
    @Nonnull
    UserManager createManager(@Nonnull ScopeInfo scope, @Nonnull Extension config);
}
