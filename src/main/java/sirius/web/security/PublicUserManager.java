/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.di.std.Register;
import sirius.kernel.extensions.Extension;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
import java.util.Collections;

/**
 * A simple user manager which always returns the same user with a defined set of roles.
 * <p>
 * Essentially this user manager make all functionality public accessible by always returning a user which has a
 * defined set of roles (therefore one can of course disable some functions entirely).
 * </p>
 * <p>
 * This roles granted can be controlled by two config entries. On is <tt>security.publicRoles</tt> which
 * also affects all other user managers. The other is <tt>defaultRoles</tt> which has to be defined within
 * the scope.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/06
 */
public class PublicUserManager extends GenericUserManager {

    private final UserInfo user;

    @Register(name = "public")
    public static class Factory implements UserManagerFactory {

        @Nonnull
        @Override
        public UserManager createManager(@Nonnull ScopeInfo scope, @Nonnull Extension config) {
            return new PublicUserManager(scope, config);
        }

    }

    protected PublicUserManager(ScopeInfo scope, Extension config) {
        super(scope, config);
        this.user = new UserInfo(null, null, "(public)", "(public)", "", transformRoles(Collections.emptySet()), null);
    }

    @Nonnull
    @Override
    public UserInfo bindToRequest(@Nonnull WebContext ctx) {
        return user;
    }

    @Override
    protected UserInfo findUserByName(WebContext ctx, String user) {
        return null;
    }

    @Override
    protected UserInfo findUserByCredentials(WebContext ctx, String user, String password) {
        throw new UnsupportedOperationException();
    }

    @Override
    protected Object getUserObject(UserInfo u) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void attachToSession(@Nonnull UserInfo user, @Nonnull WebContext ctx) {

    }

    @Override
    public void detachFromSession(@Nonnull UserInfo user, @Nonnull WebContext ctx) {

    }
}
