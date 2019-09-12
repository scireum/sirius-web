/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.collect.Sets;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Monoflop;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.health.Log;
import sirius.kernel.nls.NLS;
import sirius.kernel.settings.Extension;

import javax.annotation.Nullable;
import java.lang.reflect.AnnotatedElement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

    private static class Profile {
        private final String name;
        private final Set<String> permissionsToAdd;
        private final Set<String> permissionsToRemove;

        Profile(String name, Set<String> permissionsToAdd, Set<String> permissionsToRemove) {
            this.name = name;
            this.permissionsToAdd = permissionsToAdd;
            this.permissionsToRemove = permissionsToRemove;
        }

        protected void apply(Set<String> permissions) {
            if (hasPermission(name, permissions::contains)) {
                permissions.addAll(permissionsToAdd);
                permissions.removeAll(permissionsToRemove);
            }
        }

        protected static Profile compile(Extension extension) {
            Set<String> permissionsToAdd = new HashSet<>();
            Set<String> permissionsToRemove = new HashSet<>();

            for (Map.Entry<String, Object> permission : extension.getContext().entrySet()) {
                if (Boolean.TRUE.equals(permission.getValue())) {
                    permissionsToAdd.add(permission.getKey());
                }
                if (Boolean.FALSE.equals(permission.getValue())) {
                    permissionsToRemove.add(permission.getKey());
                }
            }

            return new Profile(extension.getId(), permissionsToAdd, permissionsToRemove);
        }
    }

    protected static List<Profile> profilesCache;

    @ConfigValue("security.publicRoles")
    protected static List<String> publicRoles;

    private Permissions() {
    }

    private static List<Profile> getProfiles() {
        if (profilesCache == null) {
            loadProfiles();
        }

        return profilesCache;
    }

    private static void loadProfiles() {
        List<Profile> profiles = new ArrayList<>();

        for (Extension ext : Sirius.getSettings().getExtensions("security.profiles")) {
            profiles.add(Profile.compile(ext));
            validateProfile(ext);
        }

        profilesCache = profiles;
    }

    private static boolean validateProfile(Extension profile) {
        Monoflop warningOccured = Monoflop.create();
        for (String permission : profile.getContext().keySet()) {
            Extension otherProfile = Sirius.getSettings().getExtension("security.profiles", permission);
            if (otherProfile == null) {
                continue;
            }
            if (otherProfile.compareTo(profile) < 0) {
                warningOccured.toggle();
                LOG.WARN("Profile '%s' refers to a profile wich is applied ealier than itself ('%s'). "
                         + "Therefore the profiles will not be resolved completely. Fix this by adding priorities.",
                         profile.getId(),
                         otherProfile.getId());
            }
        }
        return warningOccured.isToggled();
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
     * Adds all public roles and then applies all known profiles.
     * <p>
     * Applies all known profiles on the given set of roles/permissions just like {@link #applyProfiles(java.util.Set)}.
     * Also all public roles defined in <tt>security.publicRoles</tt> are included to the roles set before profiles
     * are expanded.
     *
     * @param roles the list of permissions and or profiles to expand
     * @return an effective list of permissions based on the profiles defined in <tt>security.profiles</tt> and the
     * public roles defined in <tt>security.publicRoles</tt>
     */
    public static Set<String> applyProfilesAndPublicRoles(Collection<String> roles) {
        Set<String> allRoles = new HashSet<>(roles);
        if (publicRoles != null) {
            allRoles.addAll(publicRoles);
        }
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
     * Determines if the permission expression is contained for an object.
     * <p>
     * Next to plain permission names, permissions can also negated using <tt>!permission</tt> and on top of that,
     * whole logical expressions in DNF (disjuctive normal form) can be passed in.
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

        if (DISABLED.equals(permissionExpression)) {
            return false;
        }

        if (ENABLED.equals(permissionExpression)) {
            return true;
        }

        for (String orClause : permissionExpression.split(",")) {
            if (permissionsFullfilled(orClause, containsPermission)) {
                return true;
            }
        }

        return false;
    }

    protected static boolean permissionsFullfilled(String permisssionString, Predicate<String> containsPermission) {
        for (String permission : permisssionString.split("\\+")) {
            if (!permissionFullfilled(permission, containsPermission)) {
                return false;
            }
        }
        return true;
    }

    protected static boolean permissionFullfilled(String permission, Predicate<String> containsPermission) {
        if (permission.startsWith("!")) {
            return containsPermission == null || !containsPermission.test(permission.substring(1));
        } else {
            return containsPermission != null && containsPermission.test(permission);
        }
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
