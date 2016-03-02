/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.collect.Sets;
import com.typesafe.config.Config;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Register;
import sirius.kernel.extensions.Extension;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
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
        if (sessionStorage == SESSION_STORAGE_TYPE_CLIENT) {
            UserContext.LOG.WARN(
                    "SSOUserManager (sso) for scope %s does not support 'client' as session type! Switching to 'server'.",
                    scope.getScopeType());
            sessionStorage = SESSION_STORAGE_TYPE_SERVER;
        }
        parseRoles = config.get("parseRoles").asBoolean(true);
    }

    @Override
    protected String computeSSOHashInput(WebContext ctx, String user, Tuple<String, String> challengeResponse) {
        if (ctx.get("roles").isFilled()) {
            return super.computeSSOHashInput(ctx, user, challengeResponse) + ctx.get("roles").asString();
        }
        return super.computeSSOHashInput(ctx, user, challengeResponse);
    }

    @Override
    protected UserInfo findUserByName(WebContext ctx, String user) {
        Set<String> roles;
        if (parseRoles) {
            roles = ctx.get("roles").asOptionalString().map(this::parseRolesString).orElseGet(() -> Sets.newTreeSet());
        } else {
            roles = Sets.newTreeSet();
        }
        roles.add(UserInfo.PERMISSION_LOGGED_IN);
        return new UserInfo(null,
                            null,
                            user,
                            user,
                            null,
                            null,
                            transformRoles(roles, ctx.isTrusted()),
                            ui -> getUserConfig(getScopeConfig(), ui),
                            null);
    }

    private Set<String> parseRolesString(String rolesString) {
        return Arrays.asList(rolesString.split(","))
                     .stream()
                     .map(String::trim)
                     .filter(Strings::isEmpty)
                     .collect(Collectors.toSet());
    }

    @Override
    protected UserInfo findUserByCredentials(WebContext ctx, String user, String password) {
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
