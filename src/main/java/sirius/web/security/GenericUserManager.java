/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.base.Charsets;
import com.google.common.collect.Sets;
import com.google.common.hash.HashFunction;
import com.google.common.hash.Hashing;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.extensions.Extension;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.web.controller.Message;
import sirius.web.http.WebContext;
import sirius.web.http.session.ServerSession;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Base class for various implementations of {@link sirius.web.security.UserManager}.
 * <p>
 * Provides session handling and roles expansion using profiles (security.profiles).
 */
public abstract class GenericUserManager implements UserManager {

    /**
     * A SSO token might be one hour off and will still be accepted. This is defined by this constant.
     */
    protected static final long SSO_GRACE_PERIOD_IN_SECONDS = 60 * 60;

    /*
     * Defines the name used to signal that the user should be stored in the server session
     */
    protected static final String SESSION_STORAGE_TYPE_SERVER = "server";

    /*
     * Defines the name used to signal that the user should be stored in the client session (cookie)
     */
    protected static final String SESSION_STORAGE_TYPE_CLIENT = "client";

    protected final ScopeInfo scope;
    protected final Extension config;
    protected final String hashFunction;
    protected String sessionStorage;
    protected boolean ssoEnabled;
    protected String ssoSecret;
    protected List<String> defaultRoles;
    protected List<String> trustedRoles;
    protected UserInfo defaultUser;

    @SuppressWarnings("unchecked")
    protected GenericUserManager(ScopeInfo scope, Extension config) {
        this.scope = scope;
        this.config = config;
        this.sessionStorage = config.get("sessionStorage").asString().intern();
        this.ssoSecret = config.get("ssoSecret").asString();
        this.hashFunction = config.get("hashFunction").asString("md5");
        this.ssoEnabled = Strings.isFilled(ssoSecret) && config.get("ssoEnabled").asBoolean(false);
        this.defaultRoles = config.get("defaultRoles").get(List.class, Collections.emptyList());
        this.trustedRoles = config.get("trustedRoles").get(List.class, Collections.emptyList());
        this.defaultUser = new UserInfo(null,
                                        null,
                                        "(nobody)",
                                        "(nobody)",
                                        null,
                                        null,
                                        Permissions.applyProfilesAndPublicRoles(Collections.emptySet()),
                                        null);
    }

    protected abstract UserInfo findUserByName(WebContext ctx, String user);

    protected abstract UserInfo findUserByCredentials(WebContext ctx, String user, String password);

    protected abstract Object getUserObject(UserInfo u);

    @Nonnull
    @Override
    public UserInfo bindToRequest(@Nonnull WebContext ctx) {
        UserInfo result = findUserInSession(ctx);
        if (result != null) {

            return result;
        }

        try {
            result = loginViaUsernameAndPassword(ctx);
            if (result != null) {
                recordUserLogin(ctx, result);
                return result;
            }
        } catch (HandledException e) {
            UserContext.message(Message.error(e.getMessage()));
        } catch (Exception e) {
            UserContext.message(Message.error(Exceptions.handle(UserContext.LOG, e)));
        }

        result = loginViaSSOToken(ctx);
        if (result != null) {
            recordUserLogin(ctx, result);
            return result;
        }

        return defaultUser;
    }

    protected void recordUserLogin(WebContext ctx, UserInfo user) {
    }

    /*
     * Tries to perform a login using "user" and "token" (single sign-on)
     */
    private UserInfo loginViaSSOToken(WebContext ctx) {
        if (!ssoEnabled) {
            return null;
        }
        if (ctx.get("user").isFilled() && ctx.get("token").isFilled()) {
            ctx.hidePost();
            String user = ctx.get("user").trim();
            String token = ctx.get("token").trim();

            UserInfo result = findUserByName(ctx, user);
            if (result != null) {
                // An SSO token is TIMESTAMP:MD5
                Tuple<String, String> challengeResponse = Strings.split(token, ":");
                // Verify age...
                if (Value.of(challengeResponse.getFirst()).asLong(0)
                    > (System.currentTimeMillis() / 1000) - SSO_GRACE_PERIOD_IN_SECONDS) {
                    // Verify timestamp...
                    if (getSSOHashFunction().hashBytes(computeSSOHashInput(ctx, user, challengeResponse).getBytes(
                            Charsets.UTF_8)).toString().equals(challengeResponse.getSecond())) {
                        log("SSO-Login of %s succeeded with token: %s", user, token);
                        return result;
                    } else {
                        log("SSO-Login of %s failed due to invalid hash in token: %s", user, token);
                    }
                } else {
                    log("SSO-Login of %s failed due to outdated timestamp in token: %s", user, token);
                }
            }
            UserContext.message(Message.error(NLS.get("GenericUserManager.invalidSSO")));
        }
        return null;
    }

    /**
     * Determines which hash function is used to compute and verify SSO tokens.
     *
     * @return the hash function to use for single sign-on tokens
     */
    protected HashFunction getSSOHashFunction() {
        if ("md5".equalsIgnoreCase(hashFunction)) {
            return Hashing.md5();
        } else if ("sha1".equalsIgnoreCase(hashFunction)) {
            return Hashing.sha1();
        } else {
            throw Exceptions.handle()
                            .to(UserContext.LOG)
                            .withSystemErrorMessage(
                                    "No valid hash function was given ('%s'). Cannot compute SSO token. Pick either md5 or sha1",
                                    hashFunction)
                            .handle();
        }
    }

    /**
     * Computes the input for the hash function used to generate the auth hash.
     *
     * @param ctx               the current request
     * @param user              the name of the user
     * @param challengeResponse the challenge and response pair provided by the client
     * @return the input string used by the hash function
     */
    protected String computeSSOHashInput(WebContext ctx, String user, Tuple<String, String> challengeResponse) {
        return ssoSecret + challengeResponse.getFirst() + user;
    }

    /**
     * Applies profile transformations and adds default roles to the set of given roles.
     *
     * @param roles   the roles granted to a user
     * @param trusted determines if the user is considered a trusted user
     *                (Usually determined via {@link sirius.web.http.WebContext#isTrusted()}).
     * @return a set of permissions which contain the given roles as well as the default roles and profile
     * transformations
     */
    protected Set<String> transformRoles(Collection<String> roles, boolean trusted) {
        Set<String> allRoles = Sets.newTreeSet(roles);
        allRoles.addAll(defaultRoles);
        if (trusted) {
            allRoles.addAll(trustedRoles);
        }

        return Permissions.applyProfilesAndPublicRoles(allRoles);
    }

    /**
     * Used to write a debug log.
     * <p>
     * Automatically contains the name of the user manager and the current scope.
     *
     * @param pattern the pattern used for logging
     * @param params  the parameters applied to the pattern
     */
    protected void log(String pattern, Object... params) {
        UserContext.LOG.FINE("UserManager '%s' for scope '%s' (%s) - %s",
                             getClass().getSimpleName(),
                             scope.getScopeId(),
                             scope.getScopeType(),
                             Strings.apply(pattern, params));
    }

    /*
     * Tries to perform a login using "user" and "password".
     */
    private UserInfo loginViaUsernameAndPassword(WebContext ctx) {
        if (ctx.get("user").isFilled() && ctx.get("password").isFilled()) {
            ctx.hidePost();
            String user = ctx.get("user").trim();
            String password = ctx.get("password").trim();

            UserInfo result = findUserByCredentials(ctx, user, password);
            if (result != null) {
                log("Login of %s succeeded using password", user);
                return result;
            }
            log("Login of %s failed using password", user);
            UserContext.message(Message.error(NLS.get("GenericUserManager.invalidLogin")));
        }
        return null;
    }

    /*
     * Tries to find a user in the current session
     */
    private UserInfo findUserInSession(WebContext ctx) {
        if (sessionStorage == SESSION_STORAGE_TYPE_SERVER) {
            if (ctx.getServerSession(false).isPresent()) {
                Value userId = ctx.getServerSession().getValue(scope.getScopeId() + "-user-id");
                if (userId.isFilled()) {
                    return new UserInfo(ctx.getServerSession().getValue(scope.getScopeId() + "-tenant-id").asString(),
                                        ctx.getServerSession().getValue(scope.getScopeId() + "-tenant-name").asString(),
                                        userId.asString(),
                                        ctx.getServerSession().getValue(scope.getScopeId() + "-user-name").asString(),
                                        ctx.getServerSession().getValue(scope.getScopeId() + "-user-email").asString(),
                                        ctx.getServerSession().getValue(scope.getScopeId() + "-user-lang").asString(),
                                        computeRoles(ctx, userId.asString()),
                                        u -> getUserObject(u));
                }
            }
        } else if (sessionStorage == SESSION_STORAGE_TYPE_CLIENT) {
            Value userId = ctx.getSessionValue(scope.getScopeId() + "-user-id");
            if (userId.isFilled()) {
                return new UserInfo(ctx.getSessionValue(scope.getScopeId() + "-tenant-id").asString(),
                                    ctx.getSessionValue(scope.getScopeId() + "-tenant-name").asString(),
                                    userId.asString(),
                                    ctx.getSessionValue(scope.getScopeId() + "-user-name").asString(),
                                    ctx.getSessionValue(scope.getScopeId() + "-user-email").asString(),
                                    ctx.getSessionValue(scope.getScopeId() + "-user-lang").asString(),
                                    computeRoles(ctx, userId.asString()),
                                    u -> getUserObject(u));
            }
        }
        return null;
    }

    /**
     * Tries to compute the roles for the given user and request.
     * <p>
     * If a server session is available, we try to load the roles from there.
     *
     * @param ctx    the current request
     * @param userId the id of the user to fetch roles for
     * @return a set of roles granted to the user or an empty set if no roles were found
     */
    @SuppressWarnings("unchecked")
    protected Set<String> computeRoles(WebContext ctx, String userId) {
        if (sessionStorage == SESSION_STORAGE_TYPE_SERVER && ctx.getServerSession(false).isPresent()) {
            return ctx.getServerSession()
                      .getValue(scope.getScopeId() + "-user-roles")
                      .get(Set.class, Collections.emptySet());
        } else {
            return Collections.emptySet();
        }
    }

    /**
     * Attaches the given user to the current session.
     * <p>
     * This will make the login persistent across requests (if session management is enabled).
     *
     * @param user the user to attach to the session
     * @param ctx  the current request to attach the user to
     */
    @Override
    public void attachToSession(@Nonnull UserInfo user, @Nonnull WebContext ctx) {
        if (sessionStorage == SESSION_STORAGE_TYPE_SERVER) {
            ServerSession sess = ctx.getServerSession();
            sess.putValue(scope.getScopeId() + "-tenant-id", user.getTenantId());
            sess.putValue(scope.getScopeId() + "-tenant-name", user.getTenantName());
            sess.putValue(scope.getScopeId() + "-user-id", user.getUserId());
            sess.putValue(scope.getScopeId() + "-user-name", user.getUserName());
            sess.putValue(scope.getScopeId() + "-user-email", user.getEmail());
            sess.putValue(scope.getScopeId() + "-user-lang", user.getLang());
            sess.putValue(ServerSession.USER, user.getUserName() + "(" + user.getEmail() + ")");
            sess.markAsUserSession();
        } else if (sessionStorage == SESSION_STORAGE_TYPE_CLIENT) {
            ctx.setSessionValue(scope.getScopeId() + "-tenant-id", user.getTenantId());
            ctx.setSessionValue(scope.getScopeId() + "-tenant-name", user.getTenantName());
            ctx.setSessionValue(scope.getScopeId() + "-user-id", user.getUserId());
            ctx.setSessionValue(scope.getScopeId() + "-user-name", user.getUserName());
            ctx.setSessionValue(scope.getScopeId() + "-user-email", user.getEmail());
            ctx.setSessionValue(scope.getScopeId() + "-user-lang", user.getLang());
        }
        storeRolesForUser(user, ctx);
    }

    /**
     * Stores the roles for the current user.
     * <p>
     * This is part of {@link #attachToSession(UserInfo, sirius.web.http.WebContext)}. As each user manager
     * decide by them self if they want to store the roles in the session or somewhere else (a cache), this
     * is extracted into its own method.
     *
     * @param user the user which roles should be stored
     * @param ctx  the current request to store the roles in
     */
    protected void storeRolesForUser(UserInfo user, WebContext ctx) {
        if (sessionStorage == SESSION_STORAGE_TYPE_SERVER) {
            Optional<ServerSession> sess = ctx.getServerSession(false);
            if (sess.isPresent()) {
                sess.get().putValue(scope.getScopeId() + "-user-roles", user.getPermissions());
            }
        }
    }

    /**
     * Removes all stored user information from the current session.
     *
     * @param user the current user - passed in, in case a cache etc. has to be cleared
     * @param ctx  the request to remove all data from
     */
    @Override
    public void detachFromSession(@Nonnull UserInfo user, @Nonnull WebContext ctx) {
        if (sessionStorage == SESSION_STORAGE_TYPE_SERVER) {
            Optional<ServerSession> s = ctx.getServerSession(false);
            if (s.isPresent()) {
                ServerSession sess = s.get();
                sess.putValue(scope.getScopeId() + "-tenant-id", null);
                sess.putValue(scope.getScopeId() + "-tenant-name", null);
                sess.putValue(scope.getScopeId() + "-user-id", null);
                sess.putValue(scope.getScopeId() + "-user-name", null);
                sess.putValue(scope.getScopeId() + "-user-email", null);
                sess.putValue(scope.getScopeId() + "-user-roles", null);
                sess.putValue(scope.getScopeId() + "-user-lang", null);
            }
        } else if (sessionStorage == SESSION_STORAGE_TYPE_CLIENT) {
            ctx.setSessionValue(scope.getScopeId() + "-tenant-id", null);
            ctx.setSessionValue(scope.getScopeId() + "-tenant-name", null);
            ctx.setSessionValue(scope.getScopeId() + "-user-id", null);
            ctx.setSessionValue(scope.getScopeId() + "-user-name", null);
            ctx.setSessionValue(scope.getScopeId() + "-user-email", null);
            ctx.setSessionValue(scope.getScopeId() + "-user-lang", null);
        }
        clearRolesForUser(user, ctx);
    }

    /**
     * Removes previously stored roles from the session.
     * <p>
     * This is part of {@link #detachFromSession(UserInfo, sirius.web.http.WebContext)} and the inverse
     * {@link #storeRolesForUser(UserInfo, sirius.web.http.WebContext)}.
     *
     * @param user the user to remove its roles from the session
     * @param ctx  the request to remove role data from
     */
    protected void clearRolesForUser(UserInfo user, WebContext ctx) {
        if (sessionStorage == SESSION_STORAGE_TYPE_SERVER) {
            Optional<ServerSession> sess = ctx.getServerSession(false);
            if (sess.isPresent()) {
                sess.get().putValue(scope.getScopeId() + "-user-roles", null);
            }
        }
    }

    @Override
    public boolean isLoginSupported() {
        return true;
    }
}
