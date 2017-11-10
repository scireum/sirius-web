/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.base.Charsets;
import com.google.common.collect.Maps;
import com.google.common.hash.Hashing;
import sirius.kernel.Sirius;
import sirius.kernel.di.std.Register;
import sirius.kernel.settings.Extension;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Uses the system configuration to authenticate users.
 * <p>
 * Users can be defined in <tt>security.users</tt>.
 */
public class ConfigUserManager extends GenericUserManager {

    private static final String CONFIG_KEY_SECURITY_USERS = "security.users";
    private Map<String, Set<String>> userRoles = Maps.newTreeMap();

    /**
     * Creates a new user manager for the given scope and configuration.
     */
    @Register(name = "config")
    public static class Factory implements UserManagerFactory {

        @Nonnull
        @Override
        public UserManager createManager(@Nonnull ScopeInfo scope, @Nonnull Extension config) {
            return new ConfigUserManager(scope, config);
        }
    }

    protected ConfigUserManager(ScopeInfo scope, Extension config) {
        super(scope, config);
    }

    @Override
    public UserInfo findUserByName(@Nullable WebContext ctx, String user) {
        Extension e = Sirius.getSettings().getExtension(CONFIG_KEY_SECURITY_USERS, user);
        if (e != null) {
            return getUserInfo(ctx, user, e);
        }
        log("Unknown user: %s", user);
        return null;
    }

    @Override
    public UserInfo findUserByCredentials(@Nullable WebContext ctx, String user, String password) {
        Extension e = Sirius.getSettings().getExtension(CONFIG_KEY_SECURITY_USERS, user);
        if (e != null && e.get("passwordHash").isFilled()) {
            if (Hashing.md5()
                       .hashBytes((e.get("salt").asString() + password).getBytes(Charsets.UTF_8))
                       .toString()
                       .equals(e.get("passwordHash").asString())) {
                return getUserInfo(ctx, user, e);
            }
        } else {
            if (e == null) {
                log("Unknown user: %s", user);
            } else {
                log("Invalid password for user: %s", user);
            }
        }
        return null;
    }

    @Override
    protected Object getUserObject(UserInfo u) {
        return Sirius.getSettings().getExtension(CONFIG_KEY_SECURITY_USERS, u.getUserId());
    }

    private UserInfo getUserInfo(@Nullable WebContext ctx, String userId, Extension e) {
        Set<String> roles = computeRoles(ctx, userId);
        return UserInfo.Builder.createUser(userId)
                               .withUsername(e.get("name").asString())
                               .withEmail(e.get("email").asString())
                               .withLang(e.get("lang").getString())
                               .withPermissions(roles)
                               .withUserSupplier(u -> e)
                               .build();
    }

    @SuppressWarnings("unchecked")
    @Override
    protected Set<String> computeRoles(@Nullable WebContext ctx, String userId) {
        Set<String> roles = userRoles.get(userId);
        if (roles == null) {
            Extension e = Sirius.getSettings().getExtension(CONFIG_KEY_SECURITY_USERS, userId);
            if (e != null) {
                roles = transformRoles(e.get("permissions").get(List.class, Collections.emptyList()), ctx.isTrusted());
                roles.add(UserInfo.PERMISSION_LOGGED_IN);
            } else {
                log("Unknown user: %s - Rejecting all roles!", userId);
                roles = Collections.emptySet();
            }
            userRoles.put(userId, roles);
        }
        return roles;
    }

    @Override
    protected void storeRolesForUser(UserInfo user, WebContext ctx) {
        // Roles are constant - no need to store them in a session...
    }

    @Override
    protected void clearRolesForUser(UserInfo user, WebContext ctx) {
        // Roles are constant - no need to store them in a session...
    }
}
