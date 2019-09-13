/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.settings.Extension;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Represents a profile defined in <tt>security.profiles</tt>.
 */
class Profile {

    /**
     * The key of the profiles-configuration in the '.conf'-files.
     */
    public static final String SECURITY_PROFILES = "security.profiles";

    private final String name;
    private final Set<String> permissionsToAdd;
    private final Set<String> permissionsToRemove;

    /**
     * Creates a Profile with the given name, a set of permissions this profile adds and a set of permissions this
     * profile removes.
     * <p>
     * This method is private, because profiles should be created via the {@link #compile(Extension)}-method.
     *
     * @param name                the name of the profile
     * @param permissionsToAdd    permissions this profile adds
     * @param permissionsToRemove permission this profiles removes
     */
    private Profile(String name, Set<String> permissionsToAdd, Set<String> permissionsToRemove) {
        this.name = name;
        this.permissionsToAdd = permissionsToAdd;
        this.permissionsToRemove = permissionsToRemove;
    }

    /**
     * Applies the profile to the given set of permissions.
     *
     * @param permissions the permissions the profile should be applied to
     */
    public void apply(Set<String> permissions) {
        if (Permissions.hasPermission(name, permissions::contains)) {
            permissions.addAll(permissionsToAdd);
            permissions.removeAll(permissionsToRemove);
        }
    }

    /**
     * Validates this profile and throws exception if problems exist.
     * <p>
     * An exception will be thrown if the profile refers to another profile applied earlier than itself.
     */
    public void validate() {
        Extension thisProfile = Sirius.getSettings().getExtension(SECURITY_PROFILES, name);
        for (String permission : thisProfile.getContext().keySet()) {
            Extension otherProfile = Sirius.getSettings().getExtension(SECURITY_PROFILES, permission);
            if (otherProfile == null || otherProfile.isDefault()) {
                continue;
            }
            if (otherProfile.compareTo(thisProfile) <= 0) {
                throw new IllegalStateException(Strings.apply(
                        "Profile '%s' refers to a profile which is applied earlier than itself ('%s'). "
                        + "Therefore the profiles will not be resolved completely. Fix this by adding priorities.",
                        thisProfile.getId(),
                        otherProfile.getId()));
            }
        }
    }

    /**
     * Compiles the given extension into a {@link Profile}.
     *
     * @param extension the extension to compile
     * @return the compiled {@link Profile}
     */
    public static Profile compile(Extension extension) {
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
