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
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;

/**
 * A simple user manager which always returns the same user with a defined set of roles.
 * <p>
 * Essentially this user manager makes all functionality public accessible by always returning a user which has a
 * defined set of roles (therefore one can of course disable some functions entirely).
 * <p>
 * This roles granted can be controlled by two config entries. One is <tt>security.publicRoles</tt> which
 * also affects all other user managers. The other is <tt>defaultRoles</tt> which has to be defined within
 * the scope.
 * <p>
 * Note that also <tt>trustedRoles</tt> can be defined to control roles which are only added to a trusted user
 * (i.e. from the local network).
 */
public class PublicUserManager extends GenericUserManager {

    private static final String PUBLIC_PLACEHOLDER = "(public)";
    private final UserInfo user;
    private final UserInfo trustedUser;

    /**
     * Used to create <tt>public</tt> user managers.
     */
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
        this.user = UserInfo.Builder.createUser(PUBLIC_PLACEHOLDER)
                                    .withUsername(PUBLIC_PLACEHOLDER)
                                    .withPermissions(transformRoles(Collections.emptyList(), false))
                                    .build();
        this.trustedUser = UserInfo.Builder.createUser(PUBLIC_PLACEHOLDER)
                                           .withUsername(PUBLIC_PLACEHOLDER)
                                           .withPermissions(transformRoles(Collections.emptyList(), false))
                                           .build();
    }

    @Nonnull
    @Override
    public UserInfo bindToRequest(@Nonnull WebContext ctx) {
        if (ctx.isTrusted()) {
            return trustedUser;
        } else {
            return user;
        }
    }

    @Override
    public UserInfo findUserByName(@Nullable WebContext ctx, String user) {
        return null;
    }

    @Override
    public UserInfo findUserByCredentials(@Nullable WebContext ctx, String user, String password) {
        return null;
    }

    @Override
    protected Object getUserObject(UserInfo u) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void attachToSession(@Nonnull UserInfo user, @Nonnull WebContext ctx) {
        // Not required - there is actually no user...
    }

    @Override
    public void detachFromSession(@Nonnull UserInfo user, @Nonnull WebContext ctx) {
        // Not required - there is actually no user...
    }

    @Override
    public boolean isLoginSupported() {
        return false;
    }
}
