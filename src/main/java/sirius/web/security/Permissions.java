/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.settings.Extension;

import javax.annotation.Nullable;
import java.lang.reflect.AnnotatedElement;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Helper class to parse permission based annotations and the expand permission profiles.
 * <p>
 * Can be used to parse all present {@link sirius.web.security.Permission} and
 * {@link sirius.web.security.NotPermission} annotations for a given element.
 * <p>
 * Can also be used to expand a permission profile defined in <tt>security.profiles</tt> into an effective
 * list of permissions.
 */
public class Permissions {

    /**
     * Represents a special permission which is never granted - therefore {@link #hasPermission(String, Set)} will
     * always return false.
     */
    private static final String DISABLED = "disabled";

    /**
     * Represents a special permission which is always granted - therefore {@link #hasPermission(String, Set)} will
     * always return true.
     */
    private static final String ENABLED = "enabled";

    protected static Map<String, Set<String>> profilesCache;

    @ConfigValue("security.publicRoles")
    protected static List<String> publicRoles;

    private Permissions() {
    }

    private static Set<String> getProfile(String role) {
        if (profilesCache == null) {
            buildProfileCache();
        }

        return profilesCache.getOrDefault(role, Collections.emptySet());
    }

    private static void buildProfileCache() {
        Map<String, Set<String>> profiles = Maps.newHashMap();

        for (Extension ext : Sirius.getSettings().getExtensions("security.profiles")) {
            Set<String> profile = compileProfile(ext);
            profiles.put(ext.getId(), profile);
        }

        profilesCache = profiles;
    }

    private static Set<String> compileProfile(Extension ext) {
        Set<String> permissions = Sets.newTreeSet();
        for (Map.Entry<String, Object> permission : ext.getContext().entrySet()) {
            if (Value.of(permission.getValue()).asBoolean()) {
                permissions.add(permission.getKey());
            }
        }

        return permissions;
    }

    private static void expand(String role, Set<String> result) {
        if (!result.contains(role)) {
            result.add(role);
            for (String subRole : getProfile(role)) {
                expand(subRole, result);
            }
        }
    }

    /**
     * Expands all permission profiles to obtain the effective set of permissions for a given list or permission and
     * profile names.
     *
     * @param roles the list of permissions and or profiles to expand
     * @return an effective list of permissions based on the profiles defined in <tt>security.profiles</tt>
     */
    public static Set<String> applyProfiles(Collection<String> roles) {
        Set<String> result = Sets.newTreeSet();
        for (String role : roles) {
            expand(role, result);
        }
        return result;
    }

    /**
     * Expands all permission profiles just like {@link #applyProfiles(java.util.Collection)}. Also all public roles
     * defined in <tt>security.publicRoles</tt> are included to the roles set before profiles are expanded.
     *
     * @param roles the list of permissions and or profiles to expand
     * @return an effective list of permissions based on the profiles defined in <tt>security.profiles</tt> and the
     * public roles defined in <tt>security.publicRoles</tt>
     */
    public static Set<String> applyProfilesAndPublicRoles(Collection<String> roles) {
        Set<String> allRoles = Sets.newTreeSet(roles);
        if (publicRoles != null) {
            allRoles.addAll(publicRoles);
        }
        return applyProfiles(allRoles);
    }

    /**
     * Parses all available permission annotations for the given element.
     *
     * @param object the element for check for permissions
     * @return a set of permissions required to execute the annotated element
     */
    public static Set<String> computePermissionsFromAnnotations(AnnotatedElement object) {
        if (!wearsPermissionAnnotation(object)) {
            return Collections.emptySet();
        }

        Set<String> result = Sets.newTreeSet();
        for (Permission p : object.getAnnotationsByType(Permission.class)) {
            result.add(p.value());
        }
        for (NotPermission p : object.getAnnotationsByType(NotPermission.class)) {
            result.add("!" + p.value());
        }
        if (object.isAnnotationPresent(LoginRequired.class)) {
            result.add(UserInfo.PERMISSION_LOGGED_IN);
        }
        return result;
    }

    /**
     * Determines if the given element wears a permission annotation.
     *
     * @param object the java element to check
     * @return <tt>true</tt> if there is a permission annotation, <tt>false</tt> otherwise
     * @see Permission
     * @see PermissionList
     * @see NotPermission
     * @see NotPermissionList
     * @see LoginRequired
     */
    public static boolean wearsPermissionAnnotation(AnnotatedElement object) {
        if (object.isAnnotationPresent(Permission.class) || object.isAnnotationPresent(NotPermission.class)) {
            return true;
        }

        if (object.isAnnotationPresent(PermissionList.class) || object.isAnnotationPresent(NotPermissionList.class)) {
            return true;
        }

        return object.isAnnotationPresent(LoginRequired.class);
    }

    /**
     * Determines if the permission expression is contained in the given permissions.
     * <p>
     * Next to plain permission names, permissions can also negated using <tt>!permission</tt> and on top of that,
     * whole logical expressions in DNF (disjuctive normal form) can be passed in.
     * <p>
     * Such a formula is a set of expressions where a <b>,</b> represents an <tt>or</tt> and a <b>+</b> represents an
     * <tt>and</tt>. An example would be "logged-in,important-customer+!locked". This would translate to "the user has
     * to be logged in or it has to be an important customer and not be locked".
     *
     * @param permissionExpression the permission expression to check
     * @param permissions          all permissions as set
     * @return <tt>true</tt> if the given permissions contains the permission expression, <tt>false</tt> otherwise
     */
    @SuppressWarnings("squid:S2259")
    @Explain("permissionExpression can not be null due to Strings.isEmpty")
    public static boolean hasPermission(@Nullable String permissionExpression, @Nullable Set<String> permissions) {
        if (Strings.isEmpty(permissionExpression)) {
            return true;
        }

        if (DISABLED.equals(permissionExpression)) {
            return false;
        }

        if (ENABLED.equals(permissionExpression)) {
            return true;
        }

        for (String orClause : permissionExpression.split(",")) {
            if (permissionsFullfilled(orClause, permissions)) {
                return true;
            }
        }

        return false;
    }

    protected static boolean permissionsFullfilled(String permisssionString, Set<String> permissions) {
        for (String permission : permisssionString.split("\\+")) {
            if (!permissionFullfilled(permission, permissions)) {
                return false;
            }
        }
        return true;
    }

    protected static boolean permissionFullfilled(String permission, Set<String> permissions) {
        if (permission.startsWith("!")) {
            return permissions == null || !permissions.contains(permission.substring(1));
        } else {
            return permissions != null && permissions.contains(permission);
        }
    }
}
