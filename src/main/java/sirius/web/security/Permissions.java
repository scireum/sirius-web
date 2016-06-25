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
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.extensions.Extension;
import sirius.kernel.extensions.Extensions;

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

    protected static Map<String, Set<String>> profilesCache;

    @ConfigValue("security.publicRoles")
    protected static List<String> publicRoles;

    private Permissions() {
    }

    private static Set<String> getProfile(String role) {
        if (profilesCache == null) {
            Map<String, Set<String>> profiles = Maps.newHashMap();

            for (Extension ext : Extensions.getExtensions("security.profiles")) {
                Set<String> permissions = Sets.newTreeSet();
                for (Map.Entry<String, Object> permission : ext.getContext().entrySet()) {
                    if (Value.of(permission.getValue()).asBoolean()) {
                        permissions.add(permission.getKey());
                    }
                }
                profiles.put(ext.getId(), permissions);
            }

            profilesCache = profiles;
        }
        return profilesCache.getOrDefault(role, Collections.emptySet());
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
        if (object.isAnnotationPresent(Permission.class)
            || object.isAnnotationPresent(NotPermission.class)
            || object.isAnnotationPresent(PermissionList.class)
            || object.isAnnotationPresent(NotPermissionList.class)
            || object.isAnnotationPresent(LoginRequired.class)) {
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
        return Collections.emptySet();
    }
}
