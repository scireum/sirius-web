/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.collect.Sets;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.settings.Extension;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Provides a user manager which only authenticates users by validating a single sign-on token.
 * <p>
 * Such a token has to contain the current timestamp along with a computed hash value using:
 * {@code MD5(ssoSecret + timestamp + username + roles)}. Note that MD5 can be replaced by SHA1 by changing
 * the config value "hashFunction".
 */
public class SSOUserManager extends GenericUserManager {

    private static final String PARAM_ROLES = "roles";

    /**
     * Used to create <tt>sso</tt> user managers.
     */
    @Register(name = "sso")
    public static class Factory implements UserManagerFactory {

        @Nonnull
        @Override
        public UserManager createManager(@Nonnull ScopeInfo scope, @Nonnull Extension config) {
            return new SSOUserManager(scope, config);
        }
    }

    private final boolean parseRoles;

    protected SSOUserManager(ScopeInfo scope, Extension config) {
        super(scope, config);
        if (SESSION_STORAGE_TYPE_CLIENT.equals(sessionStorage)) {
            UserContext.LOG.WARN(
                    "SSOUserManager (sso) for scope %s does not support 'client' as session type! Switching to 'server'.",
                    scope.getScopeType());
            sessionStorage = SESSION_STORAGE_TYPE_SERVER;
        }
        parseRoles = config.get("parseRoles").asBoolean(true);
    }

    @Override
    protected String computeSSOHashInput(String user, String timestamp) {
        WebContext ctx = CallContext.getCurrent().get(WebContext.class);
        if (ctx.isValid() && ctx.get(PARAM_ROLES).isFilled()) {
            return super.computeSSOHashInput(user, timestamp) + ctx.get(PARAM_ROLES).asString();
        }
        return super.computeSSOHashInput(user, timestamp);
    }

    @Override
    public UserInfo findUserByName(@Nullable WebContext ctx, String user) {
        Set<String> roles;
        if (ctx != null && parseRoles) {
            roles = ctx.get(PARAM_ROLES).asOptionalString().map(this::parseRolesString).orElseGet(Sets::newTreeSet);
        } else {
            roles = Sets.newTreeSet();
        }
        roles.add(UserInfo.PERMISSION_LOGGED_IN);

        return UserInfo.Builder.createUser(user)
                               .withUsername(user)
                               .withPermissions(transformRoles(roles, ctx != null && ctx.isTrusted()))
                               .withSettingsSupplier(ui -> getUserSettings(getScopeSettings(), ui))
                               .build();
    }

    private Set<String> parseRolesString(String rolesString) {
        return Arrays.stream(rolesString.split(","))
                     .map(String::trim)
                     .filter(Strings::isEmpty)
                     .collect(Collectors.toSet());
    }

    @Override
    public UserInfo findUserByCredentials(@Nullable WebContext ctx, String user, String password) {
        return null;
    }

    @Override
    protected Object getUserObject(UserInfo u) {
        return null;
    }

    @Override
    public boolean isLoginSupported() {
        return false;
    }
}
