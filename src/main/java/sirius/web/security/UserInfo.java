/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.transformers.Composable;
import sirius.kernel.di.transformers.Transformable;
import sirius.kernel.health.Exceptions;

import javax.annotation.CheckReturnValue;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Represents an user.
 * <p>
 * A user is authenticated using a {@link UserManager}. To obtain or modify the current user, use {@link
 * UserContext#getCurrentUser()} or {@link UserContext#setCurrentUser(UserInfo)}.
 */
public class UserInfo extends Composable {

    /**
     * This permission represents a user which was successfully authenticated by its user manager.
     */
    public static final String PERMISSION_LOGGED_IN = "flag-logged-in";

    /**
     * Fallback user if no user is currently available. This user has no permissions.
     */
    public static final UserInfo NOBODY = Builder.createUser("ANONYMOUS").withUsername("(no user)").build();

    protected String tenantId;
    protected String tenantName;
    protected String userId;
    protected String username;
    protected String lang;
    protected Set<String> permissions = null;
    protected Supplier<String> nameAppendixSupplier;
    protected Function<UserInfo, UserSettings> settingsSupplier;
    protected Function<UserInfo, Object> userSupplier;
    protected BiPredicate<UserInfo, String> subScopeCheck;

    /**
     * Builder pattern to create a new {@link UserInfo}.
     */
    public static class Builder {

        private UserInfo user;

        private Builder() {
        }

        /**
         * Creates a new builder and initializes it with an id for the user.
         *
         * @param id the id of the user to build.
         * @return the builder itself for fluent method calls
         */
        @CheckReturnValue
        public static Builder createUser(@Nonnull String id) {
            Builder builder = new Builder();
            builder.user = new UserInfo();
            builder.user.userId = id;
            return builder;
        }

        /**
         * Provides a method to copy everything but the permissions from the given user info.
         *
         * @param info the info to copy from
         * @return the builder itself for fluent method calls
         */
        public static Builder withUser(@Nonnull UserInfo info) {
            return createUser(info.getUserId()).withLang(info.getLang())
                                               .withUsername(info.getUserName())
                                               .withNameAppendixSupplier(info.getNameAppendixSupplier())
                                               .withTenantId(info.getTenantId())
                                               .withTenantName(info.getTenantName())
                                               .withSettingsSupplier(info.settingsSupplier)
                                               .withUserSupplier(info.userSupplier);
        }

        /**
         * Sets the name of the user.
         *
         * @param name the name of the user.
         * @return the builder itself for fluent method calls
         */
        public Builder withUsername(String name) {
            verifyState();
            user.username = name;
            return this;
        }

        /**
         * Contains a supplier for additional info, added to the Username to be used in Protocols. See {@link #getProtocolUsername()}.
         * This should be filled if multiple users can have the same {@link #username}s or if a user acts on the behalf of another user.
         *
         * @param appendixSupplier the supplier for additional info about the user.
         * @return the builder itself for fluent method calls
         */
        public Builder withNameAppendixSupplier(Supplier<String> appendixSupplier) {
            verifyState();
            user.nameAppendixSupplier = appendixSupplier;
            return this;
        }

        /**
         * Sets the id of the tenant the user belongs to.
         *
         * @param id the id of the tenant
         * @return the builder itself for fluent method calls
         */
        public Builder withTenantId(String id) {
            verifyState();
            user.tenantId = id;
            return this;
        }

        /**
         * Sets the name of the tenant the user belongs to.
         *
         * @param name the name of the tenant
         * @return the builder itself for fluent method calls
         */
        public Builder withTenantName(String name) {
            verifyState();
            user.tenantName = name;
            return this;
        }

        private void verifyState() {
            if (user == null) {
                throw new IllegalStateException("UserInfo already built.");
            }
        }

        /**
         * Sets the language code of the user.
         *
         * @param lang a two-letter language code which should be understood by {@link sirius.kernel.nls.NLS}.
         * @return the builder itself for fluent method calls
         */
        public Builder withLang(String lang) {
            verifyState();
            user.lang = lang;
            return this;
        }

        /**
         * Sets the permissions granted to the user.
         *
         * @param permissions the set of permissions granted to the user
         * @return the builder itself for fluent method calls
         */
        public Builder withPermissions(Set<String> permissions) {
            verifyState();
            user.permissions = permissions;
            return this;
        }

        /**
         * Sets a config supplier which can provide an individual configuration for the current user.
         *
         * @param settingsSupplier the function which fetches or computes the configuration for this user on demand.
         * @return the builder itself for fluent method calls
         */
        public Builder withSettingsSupplier(Function<UserInfo, UserSettings> settingsSupplier) {
            verifyState();
            user.settingsSupplier = settingsSupplier;
            return this;
        }

        /**
         * Sets a user supplier which returns the underlying user object (e.g. a database entity).
         *
         * @param userSupplier the function which fetches or computes the user object
         * @return the builder itself for fluent method calls
         */
        public Builder withUserSupplier(Function<UserInfo, Object> userSupplier) {
            verifyState();
            user.userSupplier = userSupplier;
            return this;
        }

        /**
         * Installs a checker which determines if a given sub-scope is enabled for a user.
         *
         * @param subScopeCheck the checker to install
         * @return the builder itself for fluent method calls
         */
        public Builder withSubScopeCheck(BiPredicate<UserInfo, String> subScopeCheck) {
            verifyState();
            user.subScopeCheck = subScopeCheck;
            return this;
        }

        /**
         * Builds the user, with the previously given settings.
         *
         * @return the resulting user
         */
        public UserInfo build() {
            UserInfo result = user;
            user = null;
            return result;
        }
    }

    protected UserInfo() {
    }

    /**
     * Returns the unique ID of the user.
     *
     * @return the unique ID of the user
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Returns the login or descriptive name of the user.
     *
     * @return the name of the user
     */
    public String getUserName() {
        return username;
    }

    /**
     * Returns the supplier for additional info added to the descriptive name of the user used in Protocols.
     *
     * @return the supplier for the appendix @ the user name
     */
    public Supplier<String> getNameAppendixSupplier() {
        return nameAppendixSupplier;
    }

    /**
     * The unique ID of the tenant.
     *
     * @return the unique ID the tenant the user belongs to
     */
    @Nullable
    public String getTenantId() {
        return tenantId;
    }

    /**
     * The name of the tenant.
     *
     * @return the name of the tenant the user belongs to
     */
    @Nullable
    public String getTenantName() {
        return tenantName;
    }

    /**
     * The language code of the user.
     *
     * @return the two-letter language code of the user
     */
    public String getLang() {
        return lang;
    }

    /**
     * Determines if the user has the requested permissions
     *
     * @param permissions the permissions to check
     * @return <tt>true</tt> if the user has all the requested permissions, <tt>false</tt> otherwise
     */
    public boolean hasPermissions(String... permissions) {
        for (String permission : permissions) {
            if (!hasPermission(permission)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Determines if the user has the given permission.
     * <p>
     * Next to plain permission names, permissions can also negated using <tt>!permission</tt> and on top of that,
     * whole
     * logical expressions in DNF (disjuctive normal form)can be passed in.
     * <p>
     * Such a formula is a set of expressions where a <b>,</b> represents an <tt>or</tt> and a <b>+</b> represents an
     * <tt>and</tt>. An example would be "logged-in,important-customer+!locked". This would translate to "the user has
     * to be logged in or it has to be an important customer and not be locked".
     *
     * @param permission the permission to check
     * @return <tt>true</tt> if the user has the permission, <tt>false</tt> otherwise
     */
    public boolean hasPermission(String permission) {
        return Permissions.hasPermission(permission, permissions == null ? s -> false : permissions::contains);
    }

    /**
     * Asserts that the user has the given permission.
     * <p>
     * If the user does not have the given permission, an exception is thrown.
     *
     * @param permission the permission to check
     */
    public void assertPermission(String permission) {
        if (!hasPermission(permission)) {
            throw Exceptions.createHandled()
                            .withNLSKey("UserInfo.missingPermission")
                            .set("permission", Permissions.getTranslatedPermission(permission))
                            .handle();
        }
    }

    /**
     * Determines if the user is logged in.
     *
     * @return <tt>true</tt> if the user has the permission {@link #PERMISSION_LOGGED_IN}, <tt>false</tt> otherwise
     */
    public boolean isLoggedIn() {
        return hasPermission(PERMISSION_LOGGED_IN);
    }

    /**
     * Returns the login or descriptive name of the user used in Protocols.
     * Contains additional info given via {@link #nameAppendixSupplier} to further identify the user.
     *
     * @return the name of the user
     */
    public String getProtocolUsername() {
        if (nameAppendixSupplier != null) {
            String appendix = nameAppendixSupplier.get();
            if (Strings.isFilled(appendix)) {
                return Strings.apply("%s (%s)", getUserName(), nameAppendixSupplier.get());
            }
        }

        return getUserName();
    }

    /**
     * Fetches the underlying user object of the given type.
     *
     * @param clazz the excepted type of the user object
     * @param <T>   the excepted type of the user object
     * @return the underlying user object or <tt>null</tt> if no object is present or if it has a non matching type
     */
    @SuppressWarnings("unchecked")
    @Nullable
    public <T> T getUserObject(Class<T> clazz) {
        if (userSupplier == null) {
            return null;
        }
        Object user = userSupplier.apply(this);
        if (user != null && clazz.isAssignableFrom(user.getClass())) {
            return (T) user;
        }
        return null;
    }

    @Override
    public boolean is(@Nonnull Class<?> type) {
        Transformable userObject = getUserObject(Transformable.class);
        if (userObject != null) {
            return userObject.is(type);
        }

        return super.is(type);
    }

    @Override
    public <A> Optional<A> tryAs(@Nonnull Class<A> adapterType) {
        Transformable userObject = getUserObject(Transformable.class);
        if (userObject != null) {
            return userObject.tryAs(adapterType);
        }

        return super.tryAs(adapterType);
    }

    /**
     * Returns a set of all permissions granted to the user.
     *
     * @return all permissions granted to the user.
     */
    public Set<String> getPermissions() {
        if (permissions == null) {
            return Collections.emptySet();
        }
        return Collections.unmodifiableSet(permissions);
    }

    /**
     * Obtains the user specific config.
     * <p>
     * This can be used to make parts of the system behave specific to the current scope, current tenant and user.
     *
     * @return the config object which contains all settings of the current scope, current tenant and user.
     */
    public UserSettings getSettings() {
        if (settingsSupplier == null) {
            return UserContext.getCurrentScope().getSettings();
        } else {
            return settingsSupplier.apply(this);
        }
    }

    /**
     * Determines if a sub-scope for a user is enabled or suppressed.
     * <p>
     * Sub scopes can be used e.g. to restrict a user to only be able to login to
     * the FTP server as provided by sirius-biz but not to the backend UI.
     *
     * @param subScope the sub scope to check
     * @return <tt>true</tt> if the sub scope is enabled, <tt>false</tt> otherwise
     */
    public boolean isSubScopeEnabled(String subScope) {
        if (subScopeCheck == null) {
            return true;
        }

        return subScopeCheck.test(this, subScope);
    }
}
