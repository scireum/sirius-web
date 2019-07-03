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
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.kernel.settings.Extension;
import sirius.web.controller.Message;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.time.Duration;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * Base class for various implementations of {@link sirius.web.security.UserManager}.
 * <p>
 * Provides session handling and roles expansion using profiles (security.profiles).
 */
public abstract class GenericUserManager implements UserManager {

    /**
     * /**
     * Defines the default grace period (max age of an sso timestamp) which is accepted by the system
     */
    private static final long DEFAULT_SSO_GRACE_INTERVAL = TimeUnit.HOURS.toSeconds(24);
    private static final String SUFFIX_USER_ID = "-user-id";
    private static final String SUFFIX_TENANT_ID = "-tenant-id";
    private static final String SUFFIX_TTL = "-ttl";
    private static final String PARAM_TOKEN = "token";
    private static final String PARAM_HASH = "hash";
    private static final String PARAM_TIMESTAMP = "timestamp";
    private static final String PARAM_USER = "user";
    private static final String PARAM_PASSWORD = "password";
    private static final String HASH_MD_5 = "md5";
    private static final String HASH_SHA_1 = "sha1";

    protected final ScopeInfo scope;
    protected final Extension config;
    protected final String hashFunction;
    protected final long ssoGraceInterval;
    protected boolean ssoEnabled;
    protected boolean keepLoginEnabled;
    protected String ssoSecret;
    protected List<String> publicRoles;
    protected List<String> defaultRoles;
    protected Duration loginTTL;
    protected UserInfo defaultUser;

    @SuppressWarnings("unchecked")
    protected GenericUserManager(ScopeInfo scope, Extension config) {
        this.scope = scope;
        this.config = config;
        this.ssoSecret = config.get("ssoSecret").asString();
        this.hashFunction = config.get("hashFunction").asString(HASH_MD_5);
        this.ssoEnabled = Strings.isFilled(ssoSecret) && config.get("ssoEnabled").asBoolean(false);
        this.ssoGraceInterval = config.get("ssoGraceInterval").asLong(DEFAULT_SSO_GRACE_INTERVAL);
        this.keepLoginEnabled = config.get("keepLoginEnabled").asBoolean(true);
        this.publicRoles = config.get("publicRoles").get(List.class, Collections.emptyList());
        this.defaultRoles = config.get("defaultRoles").get(List.class, Collections.emptyList());
        this.loginTTL = config.get("loginTTL").get(Duration.class, Duration.ofDays(90));
        this.defaultUser = buildDefaultUser();
    }

    protected UserInfo buildDefaultUser() {
        return UserInfo.Builder.createUser("(nobody)")
                               .withUsername("(nobody)")
                               .withPermissions(determineRolesOfDefaultUser())
                               .build();
    }

    protected Set<String> determineRolesOfDefaultUser() {
        return Permissions.applyProfilesAndPublicRoles(publicRoles);
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
     * @param scopeSettings the config of the outside scope
     * @param user          the user info which identifies the user to fetch the config for
     * @return the config specific for this user. If no config is present, the <tt>scopeSettings</tt> can be returned.
     */
    @Nonnull
    protected UserSettings getUserSettings(@Nonnull UserSettings scopeSettings, UserInfo user) {
        return scopeSettings;
    }

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
     * Updates the login cookie.
     * <p>
     * Limits the lifetime to the browser session if the login should not be kept. Furthermore the time to life and
     * login information is stored in the session.
     *
     * @param ctx       the current request
     * @param user      the user that logged in
     * @param keepLogin <tt>false</tt> if the session should be cleared when the browser session ends, <tt>true</tt>
     *                  otherwise
     */
    public void updateLoginCookie(WebContext ctx, UserInfo user, boolean keepLogin) {
        ctx.setCustomSessionCookieTTL(keepLoginEnabled && keepLogin ? null : Duration.ZERO);
        ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_ID, user.getUserId());
        ctx.setSessionValue(scope.getScopeId() + SUFFIX_TENANT_ID, user.getTenantId());
        ctx.setSessionValue(scope.getScopeId() + SUFFIX_TTL,
                            TimeUnit.SECONDS.convert(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
                            + loginTTL.getSeconds());
    }

    /**
     * Updates the login cookie.
     * <p>
     * Same as {@link #updateLoginCookie(WebContext, UserInfo)} but the 'keep login' flag is read from the context
     *
     * @param ctx  the current request
     * @param user the user that logged in
     */
    protected void updateLoginCookie(WebContext ctx, UserInfo user) {
        updateLoginCookie(ctx, user, ctx.get("keepLogin").asBoolean(false));
    }

    /*
     * Tries to perform a login using "user" and "token" (single sign-on)
     */
    private UserInfo loginViaSSOToken(WebContext ctx) {
        if (!ssoEnabled) {
            return null;
        }
        String user = ctx.get(PARAM_USER).trim();
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
            if (checkTokenValidity(user, challengeResponse)) {
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
        String token = ctx.get(PARAM_TOKEN).trim();
        if (Strings.isFilled(token)) {
            return Strings.split(token, ":");
        }

        // Supports the legacy parameters hash and timestamp...
        String hash = ctx.get(PARAM_HASH).trim();
        String timestamp = ctx.get(PARAM_TIMESTAMP).trim();
        if (Strings.isFilled(hash) && Strings.isFilled(timestamp)) {
            return Tuple.create(timestamp, hash);
        }

        return null;
    }

    private boolean checkTokenTTL(long timestamp, long maxTtl) {
        return Math.abs(timestamp - (System.currentTimeMillis() / 1000)) < maxTtl;
    }

    private boolean checkTokenValidity(String user, Tuple<String, String> challengeResponse) {
        return getSSOHashFunction().hashBytes(computeSSOHashInput(user,
                                                                  challengeResponse.getFirst()).getBytes(Charsets.UTF_8))
                                   .toString()
                                   .equalsIgnoreCase(challengeResponse.getSecond());
    }

    /**
     * Computes an auth token which can be used to perform an SSO Login.
     * <p>
     * If enabled, the computed token can be passed in using the <tt>token</tt> field.
     *
     * @param username the username to generate a token for
     * @return a login token to be used for SSO
     */
    public String computeSSOToken(String username) {
        String timestamp = String.valueOf(System.currentTimeMillis() / 1000);
        return timestamp + ":" + getSSOHashFunction().hashBytes(computeSSOHashInput(username, timestamp).getBytes(
                Charsets.UTF_8)).toString();
    }

    /**
     * Determines which hash function is used to compute and verify SSO tokens.
     *
     * @return the hash function to use for single sign-on tokens
     */
    protected HashFunction getSSOHashFunction() {
        if (HASH_MD_5.equalsIgnoreCase(hashFunction)) {
            return Hashing.md5();
        } else if (HASH_SHA_1.equalsIgnoreCase(hashFunction)) {
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
     * @param user      the name of the user
     * @param timestamp the timestamp used as challenge
     * @return the input string used by the hash function
     */
    protected String computeSSOHashInput(String user, String timestamp) {
        return user + timestamp + ssoSecret;
    }

    /**
     * Applies profile transformations and adds default roles to the set of given roles.
     *
     * @param roles the roles granted to a user
     * @return a set of permissions which contain the given roles as well as the default roles and profile
     * transformations
     */
    protected Set<String> transformRoles(Collection<String> roles) {
        Set<String> allRoles = Sets.newTreeSet(roles);
        allRoles.addAll(defaultRoles);

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
        if (ctx.get(PARAM_USER).isFilled() && ctx.getFirstFilled(PARAM_PASSWORD, PARAM_TOKEN).isFilled()) {
            ctx.hidePost();
            String user = ctx.get(PARAM_USER).trim();
            String password = ctx.getFirstFilled(PARAM_PASSWORD, PARAM_TOKEN).trim();

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
        Value userId = ctx.getSessionValue(scope.getScopeId() + SUFFIX_USER_ID);
        String tenantId = ctx.getSessionValue(scope.getScopeId() + SUFFIX_TENANT_ID).asString();
        Long ttl = ctx.getSessionValue(scope.getScopeId() + SUFFIX_TTL).getLong();

        if (ttl != null && ttl < TimeUnit.SECONDS.convert(System.currentTimeMillis(), TimeUnit.MILLISECONDS)) {
            return null;
        }

        if (!userId.isFilled() || !isUserStillValid(userId.asString(), ctx)) {
            return null;
        }

        Set<String> roles = computeRoles(ctx, userId.asString());
        if (roles == null) {
            return null;
        }

        return UserInfo.Builder.createUser(userId.asString())
                               .withUsername(computeUsername(ctx, userId.asString()))
                               .withTenantId(tenantId)
                               .withTenantName(computeTenantname(ctx, tenantId))
                               .withLang(computeLang(ctx, userId.asString()))
                               .withPermissions(roles)
                               .withSettingsSupplier(ui -> getUserSettings(getScopeSettings(), ui))
                               .withUserSupplier(this::getUserObject)
                               .build();
    }

    /**
     * Determines if the cached user object for the given ID is still valid.
     * <p>
     * The method has to check the session data by itself.
     *
     * @param userId the user id to check
     * @param ctx    the current request for which the check is performed
     * @return <tt>true</tt> if the user is still valid, false otherwise
     */
    protected boolean isUserStillValid(String userId, WebContext ctx) {
        return true;
    }

    /**
     * Boilerplate for fetching the settings of the current scope.
     *
     * @return the settings of the current scope
     */
    protected UserSettings getScopeSettings() {
        return UserContext.getCurrentScope().getSettings();
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
    @Nullable
    protected abstract Set<String> computeRoles(@Nullable WebContext ctx, String userId);

    /**
     * Compues the name of the given user and request.
     *
     * @param ctx    the current request
     * @param userId the id of the user to fetch the name for
     * @return the name of the user
     */
    @Nonnull
    protected abstract String computeUsername(@Nullable WebContext ctx, String userId);

    /**
     * Compues the name of the given tenant and request.
     *
     * @param ctx      the current request
     * @param tenantId the id of the tenant to fetch the name for
     * @return the name of the tenant
     */
    @Nonnull
    protected abstract String computeTenantname(@Nullable WebContext ctx, String tenantId);

    /**
     * Compues the langange code of the given user and request.
     *
     * @param ctx    the current request
     * @param userId the id of the user to fetch the language for
     * @return the language code for the user
     */
    @Nonnull
    protected abstract String computeLang(WebContext ctx, String userId);

    /**
     * Removes all stored user information from the current session.
     *
     * @param ctx the request to remove all data from
     */
    @Override
    public void logout(@Nonnull WebContext ctx) {
        ctx.setSessionValue(scope.getScopeId() + SUFFIX_TENANT_ID, null);
        ctx.setSessionValue(scope.getScopeId() + SUFFIX_USER_ID, null);
        ctx.setSessionValue(scope.getScopeId() + SUFFIX_TTL, null);
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
