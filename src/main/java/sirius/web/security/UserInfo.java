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

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Set;
import java.util.function.Function;

/**
 * Created by aha on 20.06.14.
 */
public class UserInfo implements Adaptable {

    public static final String PERMISSION_LOGGED_IN = "flag-logged-in";

    public static final UserInfo NOBODY =
            new UserInfo(null, null, "ANONYMOUS", "(no user)", "", null, null, null, null);
    public static final UserInfo GOD_LIKE =
            new UserInfo(null, null, "ADMIN", "(admin)", "", null, Collections.singleton("*"), null, null);

    private String tenantId;
    private String tenantName;
    private String userId;
    private String username;
    private String eMail;
    private String lang;
    private Set<String> permissions = null;
    private Function<UserInfo, Config> configSupplier;
    private boolean allPermissions = false;
    private Function<UserInfo, Object> userSupplier;

    public UserInfo(String tenantId,
                    String tenantName,
                    String userId,
                    String username,
                    String eMail,
                    @Nullable String lang,
                    Set<String> permissions,
                    Function<UserInfo, Config> configSupplier,
                    Function<UserInfo, Object> userSupplier) {
        this.tenantId = tenantId;
        this.tenantName = tenantName;
        this.userId = userId;
        this.username = username;
        this.eMail = eMail;
        this.lang = lang;
        this.permissions = permissions;
        this.configSupplier = configSupplier;
        this.allPermissions = permissions != null && permissions.contains("*");
        this.userSupplier = userSupplier;
    }

    public String getUserId() {
        return userId;
    }

    public String getUserName() {
        return username;
    }

    public String getTenantId() {
        return tenantId;
    }

    public String getTenantName() {
        return tenantName;
    }

    public String getEmail() {
        return eMail;
    }

    public String getLang() {
        return lang;
    }

    public boolean hasPermissions(String... permissions) {
        for (String permission : permissions) {
            if (!hasPermission(permission)) {
                return false;
            }
        }

        return true;
    }

    public boolean hasPermission(String permission) {
        if (Strings.isEmpty(permission)) {
            return true;
        }
        if (permission.startsWith("!")) {
            return permissions == null || !permissions.contains(permission.substring(1));
        } else {
            return allPermissions || (permissions != null && permissions.contains(permission));
        }
    }

    public void assertPermission(String permission) {
        if (!hasPermission(permission)) {
            throw Exceptions.createHandled()
                            .withNLSKey("UserInfo.missingPermission")
                            .set("permission", permission)
                            .handle();
        }
    }

    public boolean isLoggedIn() {
        return hasPermission(PERMISSION_LOGGED_IN);
    }

    @SuppressWarnings("unchecked")
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

    public Set<String> getPermissions() {
        return Collections.unmodifiableSet(permissions);
    }

    public Config getConfig() {
        if (configSupplier == null) {
            return UserContext.getCurrentScope().getConfig();
        } else {
            return configSupplier.apply(this);
        }
    }
}
