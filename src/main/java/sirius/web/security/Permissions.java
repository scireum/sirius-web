/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.Sirius;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.health.Log;
import sirius.kernel.nls.NLS;
import sirius.kernel.settings.Extension;

import javax.annotation.Nullable;
import java.lang.reflect.AnnotatedElement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;

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
     * Represents a special permission which is never granted - therefore {@link #hasPermission(String, Predicate)} will
     * always return false.
     */
    private static final String DISABLED = "disabled";

    /**
     * Represents a special permission which is always granted - therefore {@link #hasPermission(String, Predicate)}
     * will always return true.
     */
    private static final String ENABLED = "enabled";

    private static final Log LOG = Log.get("permissions");

    protected static List<Profile> profilesCache;

    private Permissions() {
    }

    /**
     * Fetches all {@link Profile profiles} known to the system.
     *
     * @return the list of all known profiles
     */
    public static List<Profile> getAvailableProfiles() {
        return Collections.unmodifiableList(getProfiles());
    }

    /**
     * Returns a list of all permissions along with their description.
     *
     * @return a list of permission/description pairs
     */
    public static List<Tuple<String, String>> getAllPermissions() {
        return Tuple.fromMap(Sirius.getSettings().getMap("security.permissions"))
                    .stream()
                    .sorted(Comparator.comparing(Tuple::getFirst))
                    .toList();
    }

    private static List<Profile> getProfiles() {
        if (profilesCache == null) {
            profilesCache = loadProfiles();
        }

        return profilesCache;
    }

    private static List<Profile> loadProfiles() {
        List<Profile> profiles = new ArrayList<>();

        for (Extension ext : Sirius.getSettings().getExtensions(Profile.SECURITY_PROFILES)) {
            Profile profile = Profile.compile(ext);
            profiles.add(profile);
            try {
                profile.validate();
            } catch (Exception e) {
                LOG.WARN(e.getMessage());
            }
        }

        return profiles;
    }

    /**
     * Applies all known profiles on the given set of roles/permissions.
     * <p>
     * Applies the profiles as defined in <tt>security.profiles</tt>.
     *
     * @param permissions the set of permissions and or profiles to expand
     */
    public static void applyProfiles(Set<String> permissions) {
        for (Profile profile : getProfiles()) {
            profile.apply(permissions);
        }
    }

    /**
     * Adds all known profiles by creating and enhancing a new roles set.
     * <p>
     * Applies all known profiles on the given set of roles/permissions just like {@link #applyProfiles(java.util.Set)}.
     *
     * @param roles the list of permissions and or profiles to expand
     * @return an effective list of permissions based on the profiles defined in <tt>security.profiles</tt>
     */
    public static Set<String> copyAndApplyProfiles(Collection<String> roles) {
        Set<String> allRoles = new HashSet<>(roles);
        applyProfiles(allRoles);

        return allRoles;
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

        Set<String> result = new TreeSet<>();
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
     * Determines if the permission expression is contained for an object.
     * <p>
     * Next to plain permission names, permissions can also be negated using <tt>!permission</tt> and on top of that,
     * whole logical expressions in DNF (disjunctive normal form) can be passed in.
     * <p>
     * Such a formula is a set of expressions where a <b>,</b> represents an <tt>or</tt> and a <b>+</b> represents an
     * <tt>and</tt>. An example would be "logged-in,important-customer+!locked". This would translate to "the user has
     * to be logged in or it has to be an important customer and not be locked".
     *
     * @param permissionExpression the permission expression to check
     * @param containsPermission   determines if a single permission is contained in the object
     * @return <tt>true</tt> if the given permissions contains the permission expression, <tt>false</tt> otherwise
     */
    @SuppressWarnings("squid:S2259")
    @Explain("permissionExpression can not be null due to Strings.isEmpty")
    public static boolean hasPermission(@Nullable String permissionExpression,
                                        @Nullable Predicate<String> containsPermission) {
        if (Strings.isEmpty(permissionExpression)) {
            return true;
        }

        for (String orClause : permissionExpression.split(",")) {
            if (permissionsFulfilled(orClause, containsPermission)) {
                return true;
            }
        }

        return false;
    }

    protected static boolean permissionsFulfilled(String permissionString, Predicate<String> containsPermission) {
        for (String permission : permissionString.split("\\+")) {
            if (!permissionFulfilled(permission, containsPermission)) {
                return false;
            }
        }
        return true;
    }

    protected static boolean permissionFulfilled(String permission, Predicate<String> containsPermission) {
        if (permission.startsWith("!")) {
            return !permissionFulfilled(permission.substring(1), containsPermission);
        }
        if (DISABLED.equals(permission)) {
            return false;
        }
        if (ENABLED.equals(permission)) {
            return true;
        }
        return containsPermission != null && containsPermission.test(permission);
    }

    /**
     * Trys to translate the given permission.
     *
     * @param permission the permission to translate
     * @return the translated permission or the permission string if it could not be translated
     */
    public static String getTranslatedPermission(String permission) {
        return NLS.getIfExists("Permission." + permission, null).orElse(permission);
    }

    /**
     * Returns the description of the permission or an empty string.
     *
     * @param permission the permission to describe
     * @return the permission description if available or an empty string
     */
    public static String getPermissionDescription(String permission) {
        return NLS.getIfExists("Permission." + permission + ".description", null).orElse("");
    }
}
