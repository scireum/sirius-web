/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.kernel.settings.Extension;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Set;

/**
 * A simple user manager which always returns the same user with a defined set of roles.
 * <p>
 * Essentially this user manager makes all functionality public accessible by always returning a user which has a
 * defined set of roles (therefore one can of course disable some functions entirely).
 * <p>
 * This roles granted can be controlled by two config entries. One is <tt>security.publicRoles</tt> which
 * also affects all other user managers. The other is <tt>defaultRoles</tt> which has to be defined within
 * the scope.
 */
public class PublicUserManager extends GenericUserManager {

    private static final String PUBLIC_PLACEHOLDER = "(public)";
    private final UserInfo user;

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
                                    .withPermissions(transformRoles(Collections.emptyList()))
                                    .build();
    }

    @Nonnull
    @Override
    public UserInfo bindToRequest(@Nonnull WebContext ctx) {
        return user;
    }

    @Override
    public UserInfo findUserByName(@Nullable WebContext ctx, String user) {
        return null;
    }

    @Override
    public UserInfo findUserByCredentials(@Nullable WebContext ctx, String user, String password) {
        return null;
    }

    @Nullable
    @Override
    public UserInfo findUserByUserId(String userId) {
        return null;
    }

    @Override
    protected Object getUserObject(UserInfo u) {
        throw new UnsupportedOperationException();
    }

    @Nullable
    @Override
    protected Set<String> computeRoles(@Nullable WebContext ctx, String userId) {
        return Collections.emptySet();
    }

    @Nonnull
    @Override
    protected String computeUsername(@Nullable WebContext ctx, String userId) {
        return PUBLIC_PLACEHOLDER;
    }

    @Nonnull
    @Override
    protected String computeTenantname(@Nullable WebContext ctx, String tenantId) {
        return "";
    }

    @Nonnull
    @Override
    protected String computeLang(WebContext ctx, String userId) {
        return NLS.getDefaultLanguage();
    }

    @Override
    public void logout(@Nonnull WebContext ctx) {
        // Not required - there is actually no user...
    }

    @Override
    public boolean isLoginSupported() {
        return false;
    }
}
