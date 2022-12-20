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
 * This roles granted can be controlled by the two scope config entries <tt>defaultRoles</tt> and <tt>publicRoles</tt>.
 * We simply combine both lists to be compatible with the config keys used by other user manages (e.g.
 * one which extend the {@link GenericUserManager}).
 */
public class PublicUserManager extends GenericUserManager {

    private static final String AUTO_LANGUAGE = "auto";
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
                                    .withPermissions(transformRoles(publicRoles))
                                    .build();
    }

    @Nonnull
    @Override
    public UserInfo bindToRequest(@Nonnull WebContext webContext) {
        return user;
    }

    @Override
    public UserInfo findUserByName(@Nullable WebContext webContext, String user) {
        return null;
    }

    @Override
    public UserInfo findUserByCredentials(@Nullable WebContext webContext, String user, String password) {
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
    protected Set<String> computeRoles(@Nullable WebContext webContext, String userId) {
        return Collections.emptySet();
    }

    @Nonnull
    @Override
    protected String computeUsername(@Nullable WebContext webContext, String userId) {
        return PUBLIC_PLACEHOLDER;
    }

    @Nonnull
    @Override
    protected String computeTenantname(@Nullable WebContext webContext, String tenantId) {
        return "";
    }

    @Nonnull
    @Override
    protected String computeLanguage(WebContext webContext, String userId) {
        String language = scope.getDefaultLanguageOrFallback();
        return AUTO_LANGUAGE.equals(language) ? NLS.getSystemLanguage() : language;
    }

    @Override
    public void logout(@Nonnull WebContext webContext) {
        // Not required - there is actually no user...
    }

    @Override
    public boolean isLoginSupported() {
        return false;
    }

    @Override
    public UserInfo createUserWithTenant(UserInfo originalUser, String tenantId) {
        return originalUser;
    }
}
