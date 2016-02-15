/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.typesafe.config.Config;
import sirius.kernel.di.std.Register;
import sirius.kernel.extensions.Extension;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
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

    private final UserInfo user;
    private final UserInfo trustedUser;

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
        this.user = new UserInfo(null,
                                 null,
                                 "(public)",
                                 "(public)",
                                 "",
                                 null,
                                 transformRoles(Collections.emptyList(), false),
                                 null,
                                 null);
        this.trustedUser = new UserInfo(null,
                                        null,
                                        "(public)",
                                        "(public)",
                                        "",
                                        null,
                                        transformRoles(Collections.emptyList(), true),
                                        null,
                                        null);
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
    protected boolean isSupportsUserConfig() {
        return false;
    }

    @Override
    protected Config getUserConfig(UserInfo u) {
        return null;
    }

    @Override
    public void attachToSession(@Nonnull UserInfo user, @Nonnull WebContext ctx) {
    }

    @Override
    public void detachFromSession(@Nonnull UserInfo user, @Nonnull WebContext ctx) {
    }

    @Override
    public boolean isLoginSupported() {
        return false;
    }
}
