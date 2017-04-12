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
import com.typesafe.config.Config;
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
import javax.annotation.Nullable;
import java.time.Duration;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * Base class for various implementations of {@link sirius.web.security.UserManager}.
 * <p>
 * Provides session handling and roles expansion using profiles (security.profiles).
 */
public abstract class GenericUserManager implements UserManager {

    /**
     * Defines the name used to store the user detail for cookie login storage
     */
    private static final String USER_COOKIE_SUFFIX = "-sirius-user";

    /**
     * Defines the name used to store the token detail for cookie login storage
     */
    private static final String TOKEN_COOKIE_SUFFIX = "-sirius-token";

    /**
     * Defines the name used to signal that the user should be stored in the server session
     */
    protected static final String SESSION_STORAGE_TYPE_SERVER = "server";

    /**
     * Defines the name used to signal that the user should be stored in the client session (cookie)
     */
    protected static final String SESSION_STORAGE_TYPE_CLIENT = "client";

    /**
     * Defines the default grace period (max age of an sso timestamp) which is accepted by the system
     */
    private static final long DEFAULT_SSO_GRACE_INTERVAL = TimeUnit.HOURS.toSeconds(24);
    private static final String SUFFIX_USER_ID = "-user-id";
    private static final String SUFFIX_USER_NAME = "-user-name";
    private static final String SUFFIX_TENANT_ID = "-tenant-id";
    private static final String SUFFIX_TENANT_NAME = "-tenant-name";
    private static final String SUFFIX_USER_EMAIL = "-user-email";
    private static final String SUFFIX_USER_LANG = "-user-lang";
    private static final String SUFFIX_USER_ROLES = "-user-roles";

    protected final ScopeInfo scope;
    protected final Extension config;
    protected final String hashFunction;
    protected final long ssoGraceInterval;
    protected String sessionStorage;
    protected boolean ssoEnabled;
    protected boolean keepLoginEnabled;
    protected String ssoSecret;
    protected List<String> defaultRoles;
    protected List<String> trustedRoles;
    protected Duration loginCookieTTL;
    protected UserInfo defaultUser;

    @SuppressWarnings("unchecked")
    protected GenericUserManager(ScopeInfo scope, Extension config) {
        this.scope = scope;
        this.config = config;
        this.sessionStorage = config.get("sessionStorage").asString().intern();
        this.ssoSecret = config.get("ssoSecret").asString();
        this.hashFunction = config.get("hashFunction").asString("md5");
        this.ssoEnabled = Strings.isFilled(ssoSecret) && config.get("ssoEnabled").asBoolean(false);
        this.ssoGraceInterval = config.get("ssoGraceInterval").asLong(DEFAULT_SSO_GRACE_INTERVAL);
        this.keepLoginEnabled = config.get("keepLoginEnabled").asBoolean(true);
        this.defaultRoles = config.get("defaultRoles").get(List.class, Collections.emptyList());
        this.trustedRoles = config.get("trustedRoles").get(List.class, Collections.emptyList());
        this.loginCookieTTL = config.get("loginCookieTTL").get(Duration.class, Duration.ofDays(90));
        this.defaultUser = UserInfo.Builder.createUser("(nobody)")
                                           .withUsername("(nobody)")
                                           .withPermissions(Permissions.applyProfilesAndPublicRoles(Collections.emptySet()))
                                           .build();
    }

    /**
     * Resolves the given user info back into the original (underlying) user object.
     *
     * @param user the user info which was passed to the outside world.
     * @return the original (underlying) user object
     */
    protected abstract Object getUserObject(UserInfo user);

    /**
     * Fetches the user specific configuration if available.
     *
     * @param scopeConfig the config of the outside scope
     * @param user        the user info which identifies the user to fetch the config for
     * @return the config specific for this user. If no config is present, the <tt>scopeConfig</tt> can be returned.
     */
    @Nonnull
    protected Config getUserConfig(@Nonnull Config scopeConfig, UserInfo user) {
        return scopeConfig;
    }

    @Nonnull
    @Override
    public UserInfo bindToRequest(@Nonnull WebContext ctx) {
        UserInfo result = findUserInSession(ctx);
        if (result != null) {
            return result;
        }

        if (SESSION_STORAGE_TYPE_SERVER.equals(sessionStorage)) {
            result = loginViaCookie(ctx);
            if (result != null) {
                recordUserLogin(ctx, result);
                return result;
            }
        }

        try {
            result = loginViaUsernameAndPassword(ctx);
            if (result != null) {
                onLogin(ctx, result);
                return result;
            }
        } catch (HandledException e) {
            UserContext.message(Message.error(e.getMessage()));
        } catch (Exception e) {
            UserContext.message(Message.error(Exceptions.handle(UserContext.LOG, e)));
        }

        result = loginViaSSOToken(ctx);
        if (result != null) {
            onLogin(ctx, result);
            return result;
        }

        return defaultUser;
    }

    @Nonnull
    @Override
    public UserInfo findUserForRequest(@Nonnull WebContext ctx) {
        UserInfo result = findUserInSession(ctx);
        if (result != null) {
            return result;
        } else {
            return defaultUser;
        }
    }

    /**
     * Invoked once a user actually performs a login via the web interface.
     *
     * @param ctx  the current request
     * @param user the user which logged in
     */
    private void onLogin(WebContext ctx, UserInfo user) {
        updateLoginCookie(ctx, user);
        recordUserLogin(ctx, user);
    }

    /**
     * Provides a method which can track logins of users.
     *
     * @param ctx  the current request
     * @param user the user which logged in
     */
    protected void recordUserLogin(WebContext ctx, UserInfo user) {
    }

    /**
     * Updates the lifetime of the login cooke if required.
     *
     * @param ctx  the current request
     * @param user the user that logged in
     */
    protected void updateLoginCookie(WebContext ctx, UserInfo user) {
        if (SESSION_STORAGE_TYPE_SERVER.equals(sessionStorage)) {
            if (isKeepLogin(ctx)) {
                ctx.setCookie(scope.getScopeId() + USER_COOKIE_SUFFIX,
                              user.getUserName().trim(),
                              loginCookieTTL.getSeconds());

                String timestamp = String.valueOf(System.currentTimeMillis() / 1000);
                String input = computeSSOHashInput(ctx, user.getUserName().trim(), new Tuple<>(timestamp, null));
                String challenge = getSSOHashFunction().hashBytes(input.getBytes(Charsets.UTF_8)).toString();

                ctx.setCookie(scope.getScopeId() + TOKEN_COOKIE_SUFFIX,
                              timestamp + ":" + challenge,
                              loginCookieTTL.getSeconds());
            }
        } else if (SESSION_STORAGE_TYPE_CLIENT.equals(sessionStorage)) {
            ctx.setCustomSessionCookieTTL(isKeepLogin(ctx) ? loginCookieTTL : Duration.ZERO);
        }
    }

    private boolean isKeepLogin(WebContext ctx) {
        return keepLoginEnabled && ctx.get("keepLogin").asBoolean(false);
    }

    /*
     * Tries to perform a login using "user" and "token" (single sign-on)
     */
    private UserInfo loginViaSSOToken(WebContext ctx) {
        if (!ssoEnabled) {
            return null;
        }
        String user = ctx.get("user").trim();
        if (Strings.isEmpty(user)) {
            return null;
        }
        Tuple<String, String> challengeResponse = extractChallengeAndResponse(ctx);
        if (challengeResponse == null) {
            return null;
        }

        ctx.hidePost();
        UserInfo result = findUserByName(ctx, user);
        if (result == null) {
            UserContext.message(Message.error(NLS.get("GenericUserManager.invalidSSO")));
            return null;
        }
        if (checkTokenTTL(Value.of(challengeResponse.getFirst()).asLong(0), ssoGraceInterval)) {
            if (checkTokenValidity(ctx, user, challengeResponse)) {
                log("SSO-Login of %s succeeded with token: %s", user, challengeResponse);
                return result;
            } else {
                log("SSO-Login of %s failed due to invalid hash in token: %s", user, challengeResponse);
            }
        } else {
            log("SSO-Login of %s failed due to outdated timestamp in token: %s", user, challengeResponse);
        }

        return null;
    }

    protected Tuple<String, String> extractChallengeAndResponse(WebContext ctx) {
        // Supports the modern parameter token which contains TIMESTAMP:MD5
        String token = ctx.get("token").trim();
        if (Strings.isFilled(token)) {
            return Strings.split(token, ":");
        }

        // Supports the legacy parameters hash and timestamp...
        String hash = ctx.get("hash").trim();
        String timestamp = ctx.get("timestamp").trim();
        if (Strings.isFilled(hash) && Strings.isFilled(timestamp)) {
            return Tuple.create(timestamp, hash);
        }

        return null;
    }

    private UserInfo loginViaCookie(WebContext ctx) {
        if (!keepLoginEnabled) {
            return null;
        }
        String user = ctx.getCookieValue(scope.getScopeId() + USER_COOKIE_SUFFIX);
        String token = ctx.getCookieValue(scope.getScopeId() + TOKEN_COOKIE_SUFFIX);

        if (Strings.isEmpty(user) || Strings.isEmpty(token)) {
            return null;
        }

        UserInfo result = findUserByName(ctx, user);
        if (result == null) {
            return null;
        }

        // The cookie token is TIMESTAMP:MD5
        Tuple<String, String> challengeResponse = Strings.split(token, ":");
        // Verify age...
        if (checkTokenTTL(Value.of(challengeResponse.getFirst()).asLong(0), loginCookieTTL.getSeconds())) {
            // Verify hash...
            if (checkTokenValidity(ctx, user, challengeResponse)) {
                log("Cookie-Login of %s succeeded with token: %s", user, token);
                return result;
            } else {
                log("Cookie-Login of %s failed due to invalid hash in token: %s", user, token);
            }
        } else {
            log("Cookie-Login of %s failed due to outdated timestamp in token: %s", user, token);
        }

        return null;
    }

    private boolean checkTokenTTL(long timestamp, long maxTtl) {
        return timestamp > (System.currentTimeMillis() / 1000) - maxTtl;
    }

    private boolean checkTokenValidity(WebContext ctx, String user, Tuple<String, String> challengeResponse) {
        return getSSOHashFunction().hashBytes(computeSSOHashInput(ctx,
                                                                  user,
                                                                  challengeResponse).getBytes(Charsets.UTF_8))
                                   .toString()
                                   .equalsIgnoreCase(challengeResponse.getSecond());
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
        return user + challengeResponse.getFirst() + ssoSecret;
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

    /**
     * Tries to fetch the current user from the current session.
     * <p>
     * This can be either the client sided session (cookie) or the server session.
     *
     * @param ctx the current request to use the session from
     * @return the user in the session or <tt>null</tt> if no user is attached
     */
    protected UserInfo findUserInSession(WebContext ctx) {
        if (SESSION_STORAGE_TYPE_SERVER.equals(sessionStorage)) {
            return findUserInServerSession(ctx);
        }

        if (SESSION_STORAGE_TYPE_CLIENT.equals(sessionStorage)) {
            return findUserInClientSession(ctx);
        }
        return null;
    }

    private UserInfo findUserInClientSession(WebContext ctx) {
        Value userId = ctx.getSessionValue(scope.getScopeId() + SUFFIX_USER_ID);
        if (!userId.isFilled() || !isUserStillValid(userId.asString())) {
            return null;
        }
        Set<String> roles = computeRoles(ctx, userId.asString());
        if (roles == null) {
            return null;
        }
        return UserInfo.Builder.createUser(userId.asString())
                               .withUsername(ctx.getSessionValue(scope.getScopeId() + SUFFIX_USER_NAME).asString())
                               .withTenantId(ctx.getSessionValue(scope.getScopeId() + SUFFIX_TENANT_ID).asString())
                               .withTenantName(ctx.getSessionValue(scope.getScopeId() + SUFFIX_TENANT_NAME).asString())
                               .withEmail(ctx.getSessionValue(scope.getScopeId() + SUFFIX_USER_EMAIL).asString())
                               .withLang(ctx.getSessionValue(scope.getScopeId() + SUFFIX_USER_LANG).asString())
                               .withPermissions(roles)
                               .withConfigSupplier(ui -> getUserConfig(getScopeConfig(), ui))
                               .withUserSupplier(this::getUserObject)
                               .build();
    }

    private UserInfo findUserInServerSession(WebContext ctx) {
        if (!ctx.getServerSession(false).isPresent()) {
            return null;
        }
        Value userId = ctx.getServerSession().getValue(scope.getScopeId() + SUFFIX_USER_ID);
        if (!userId.isFilled() || !isUserStillValid(userId.asString())) {
            return null;
        }
        Set<String> roles = computeRoles(ctx, userId.asString());
        if (roles == null) {
            return null;
        }
        return UserInfo.Builder.createUser(userId.asString())
                               .withUsername(ctx.getServerSession()
                                                .getValue(scope.getScopeId() + SUFFIX_USER_NAME)
                                                .asString())
                               .withTenantId(ctx.getServerSession()
                                                .getValue(scope.getScopeId() + SUFFIX_TENANT_ID)
                                                .asString())
                               .withTenantName(ctx.getServerSession()
                                                  .getValue(scope.getScopeId() + SUFFIX_TENANT_NAME)
                                                  .asString())
                               .withEmail(ctx.getServerSession()
                                             .getValue(scope.getScopeId() + SUFFIX_USER_EMAIL)
                                             .asString())
                               .withLang(ctx.getServerSession()
                                            .getValue(scope.getScopeId() + SUFFIX_USER_LANG)
                                            .asString())
                               .withPermissions(roles)
                               .withConfigSupplier(ui -> getUserConfig(getScopeConfig(), ui))
                               .withUserSupplier(this::getUserObject)
                               .build();
    }

    /**
     * Determines if the cached user object for the given ID is still valid.
     * <p>
     * The method has to check the session data by itself.
     *
     * @param userId the user id to check
     * @return <tt>true</tt> if the user is still valid, false otherwise
     */
    protected boolean isUserStillValid(String userId) {
        return true;
    }

    protected Config getScopeConfig() {
        return UserContext.getCurrentScope().getConfig();
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
    @Nullable
    protected Set<String> computeRoles(@Nullable WebContext ctx, String userId) {
        if (ctx != null && SESSION_STORAGE_TYPE_SERVER.equals(sessionStorage) && ctx.getServerSession(false).isPresent()) {
            return ctx.getServerSession()
                      .getValue(scope.getScopeId() + SUFFIX_USER_ROLES)
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
        if (SESSION_STORAGE_TYPE_SERVER.equals(sessionStorage)) {
            ServerSession sess = ctx.getServerSession();
            sess.putValue(scope.getScopeId() + SUFFIX_TENANT_ID, user.getTenantId());
            sess.putValue(scope.getScopeId() + SUFFIX_TENANT_NAME, user.getTenantName());
            sess.putValue(scope.getScopeId() + SUFFIX_USER_ID, user.getUserId());
            sess.putValue(scope.getScopeId() + SUFFIX_USER_NAME, user.getUserName());
            sess.putValue(scope.getScopeId() + SUFFIX_USER_EMAIL, user.getEmail());
            sess.putValue(scope.getScopeId() + SUFFIX_USER_LANG, user.getLang());
            sess.putValue(ServerSession.USER, user.getUserName() + "(" + user.getEmail() + ")");
            sess.markAsUserSession();
        } else if (SESSION_STORAGE_TYPE_CLIENT.equals(sessionStorage)) {
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_TENANT_ID, user.getTenantId());
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_TENANT_NAME, user.getTenantName());
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_ID, user.getUserId());
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_NAME, user.getUserName());
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_EMAIL, user.getEmail());
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_LANG, user.getLang());
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
        if (SESSION_STORAGE_TYPE_SERVER.equals(sessionStorage)) {
            Optional<ServerSession> sess = ctx.getServerSession(false);
            sess.ifPresent(serverSession -> serverSession.putValue(scope.getScopeId() + SUFFIX_USER_ROLES,
                                                                   user.getPermissions()));
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
        if (SESSION_STORAGE_TYPE_SERVER.equals(sessionStorage)) {
            Optional<ServerSession> s = ctx.getServerSession(false);
            if (s.isPresent()) {
                ServerSession sess = s.get();
                sess.putValue(scope.getScopeId() + SUFFIX_TENANT_ID, null);
                sess.putValue(scope.getScopeId() + SUFFIX_TENANT_NAME, null);
                sess.putValue(scope.getScopeId() + SUFFIX_USER_ID, null);
                sess.putValue(scope.getScopeId() + SUFFIX_USER_NAME, null);
                sess.putValue(scope.getScopeId() + SUFFIX_USER_EMAIL, null);
                sess.putValue(scope.getScopeId() + SUFFIX_USER_ROLES, null);
                sess.putValue(scope.getScopeId() + SUFFIX_USER_LANG, null);
            }
        } else if (SESSION_STORAGE_TYPE_CLIENT.equals(sessionStorage)) {
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_TENANT_ID, null);
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_TENANT_NAME, null);
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_ID, null);
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_NAME, null);
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_EMAIL, null);
            ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_LANG, null);
        }

        clearRolesForUser(user, ctx);

        ctx.deleteCookie(scope.getScopeId() + USER_COOKIE_SUFFIX);
        ctx.deleteCookie(scope.getScopeId() + TOKEN_COOKIE_SUFFIX);
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
        if (SESSION_STORAGE_TYPE_SERVER.equals(sessionStorage)) {
            Optional<ServerSession> sess = ctx.getServerSession(false);
            sess.ifPresent(serverSession -> serverSession.putValue(scope.getScopeId() + SUFFIX_USER_ROLES, null));
        }
    }

    @Override
    public boolean isLoginSupported() {
        return true;
    }

    @Override
    public boolean isKeepLoginSupported() {
        return keepLoginEnabled;
    }
}
