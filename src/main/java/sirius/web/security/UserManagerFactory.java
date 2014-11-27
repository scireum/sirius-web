/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.extensions.Extension;

import javax.annotation.Nonnull;

/**
 * Created by aha on 20.06.14.
 */
public interface UserManagerFactory {

    @Nonnull
    UserManager createManager(@Nonnull ScopeInfo scope, @Nonnull Extension config);

}
