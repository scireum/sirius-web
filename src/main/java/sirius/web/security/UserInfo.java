/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.typesafe.config.Config;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.morphium.Adaptable;
import sirius.kernel.health.Exceptions;

import javax.annotation.CheckReturnValue;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Set;
import java.util.function.Function;

/**
 * Represents an user.
 * <p>
 * A user is authenticated using a {@link UserManager}. To obtain or modify the current user, use {@link
 * UserContext#getCurrentUser()} or {@link UserContext#setCurrentUser(UserInfo)}.
 */
public class UserInfo implements Adaptable {

    /**
     * This permission represents a user which was successfully authenticated by its user manager.
     */
    public static final String PERMISSION_LOGGED_IN = "flag-logged-in";

    /**
     * Fallback user if no user is currently available. This user has no permissions.
     */
    public static final UserInfo NOBODY = Builder.createUser("ANONYMOUS").withUsername("(no user").build();

    private String tenantId;
    private String tenantName;
    private String userId;
    private String username;
    private String email;
    private String lang;
    private Set<String> permissions = null;
    private Function<UserInfo, Config> configSupplier;
    private Function<UserInfo, Object> userSupplier;

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
         * Sets the email address of the user.
         *
         * @param email the email address
         * @return the builder itself for fluent method calls
         */
        public Builder withEmail(String email) {
            verifyState();
            user.email = email;
            return this;
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
         * @param configSupplier the function which fetches or computes the configuration for this user on demand.
         * @return the builder itself for fluent method calls
         */
        public Builder withConfigSupplier(Function<UserInfo, Config> configSupplier) {
            verifyState();
            user.configSupplier = configSupplier;
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
     * The email address of the user
     *
     * @return the email address of the user
     */
    @Nullable
    public String getEmail() {
        return email;
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
     * If the permission starts with an "!", the check is inverted.
     *
     * @param permission the permission to check
     * @return <tt>true</tt> if the user has the permission, <tt>false</tt> otherwise
     */
    public boolean hasPermission(String permission) {
        if (Strings.isEmpty(permission)) {
            return true;
        }
        if (permission.startsWith("!")) {
            return permissions == null || !permissions.contains(permission.substring(1));
        } else {
            return permissions != null && permissions.contains(permission);
        }
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
                            .set("permission", permission)
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

    /**
     * Returns a set of all permissions granted to the user.
     *
     * @return all permissions granted to the user.
     */
    public Set<String> getPermissions() {
        return Collections.unmodifiableSet(permissions);
    }

    /**
     * Obtains the user specific config.
     * <p>
     * This can be used to make parts of the system behave specific to the current scope, current tenant and user.
     *
     * @return the config object which contains all settings of the current scope, current tenant and user.
     */
    public Config getConfig() {
        if (configSupplier == null) {
            return UserContext.getCurrentScope().getConfig();
        } else {
            return configSupplier.apply(this);
        }
    }
}
