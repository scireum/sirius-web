/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;

import sirius.kernel.Sirius;
import sirius.kernel.di.PartCollection;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;

import java.lang.reflect.Executable;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Provides a security sandbox in case <tt>Noodle</tt> is compiling a script or template provided by a user.
 * <p>
 * User code may only access methods, macros or fields which have been whitelisted. This can either be
 * performed by placing a {@link NoodleSandbox} annotation or by adding an entry in the system config in
 * <tt>scripting.sandbox</tt>.
 * <p>
 * Note that the sandbox is applied at compile time and therefore has no runtime overhead at all. Also note that
 * common scripts and templates (provided by the application or a system administrator) run without a sandbox
 * and therefore have full access to the whole JVM.
 */
@Register(classes = Sandbox.class)
public class Sandbox {

    private Set<String> denyList;
    private Set<String> allowList;

    private SandboxMode mode;

    @Parts(SandboxDetector.class)
    private PartCollection<SandboxDetector> detectors;

    /**
     * Determines the global sandbox mode for the system.
     * <p>
     * This is read from <tt>scripting.sandbox.mode</tt> in the system configuration.
     *
     * @return the currently active sandbox mode
     */
    public SandboxMode getMode() {
        if (mode == null) {
            mode = Sirius.getSettings()
                         .get("scripting.sandbox.mode")
                         .getEnum(SandboxMode.class)
                         .orElse(SandboxMode.DISABLED);
        }

        return mode;
    }

    /**
     * Determines if the sandbox check should be enforced for the given template.
     * <p>
     * For this to happen, the {@link #getMode() mode} has to be anything except <tt>DISABLED</tt> and then a
     * {@link SandboxDetector} has to mark the given resource as {@link SandboxDetector#shouldSandbox(String)}
     * (return <tt>true</tt>).
     *
     * @param resourcePath the path of the resource to check
     * @return <tt>true</tt> if the sandbox should be enabled, <tt>false</tt> otherwise
     */
    public SandboxMode determineEffectiveSandboxMode(String resourcePath) {
        String effectiveResourcePath = resourcePath.startsWith("/") ? resourcePath : "/" + resourcePath;

        if (getMode() != SandboxMode.DISABLED && detectors.getParts()
                                                          .stream()
                                                          .anyMatch(detector -> detector.shouldSandbox(
                                                                  effectiveResourcePath))) {
            return getMode();
        }

        return SandboxMode.DISABLED;
    }

    /**
     * Determines if the given annotation {@link sirius.pasta.noodle.sandbox.NoodleSandbox.Accessibility#GRANTED grants}
     * access.
     *
     * @param annotation the annotation to check
     * @return <tt>true</tt> if access is granted, <tt>false</tt> if access is rejected
     */
    public static boolean isAccessGranted(NoodleSandbox annotation) {
        return annotation.value() == NoodleSandbox.Accessibility.GRANTED;
    }

    /**
     * Determines if the given method can be invoked by user code.
     *
     * @param method the method to check
     * @return <tt>true</tt> if it can be invoked, <tt>false</tt> otherwise
     */
    public boolean canInvoke(Executable method) {
        return isControlledViaAnnotation(method).orElseGet(() -> isAllowed(method.getDeclaringClass().getName(),
                                                                           method.getName()));
    }

    private boolean isAllowed(String className, String methodName) {
        if (denyList == null) {
            loadConfig();
        }
        return !denyList.contains(className + "." + methodName) && (allowList.contains(className + "." + methodName)
                                                                    || allowList.contains(className + ".*"));
    }

    private void loadConfig() {
        Set<String> newDenyList = new HashSet<>();
        Set<String> newAllowList = new HashSet<>();

        for (Map.Entry<String, String> entry : Sirius.getSettings().getMap("scripting.sandbox.rules").entrySet()) {
            if (Boolean.parseBoolean(entry.getValue())) {
                newAllowList.add(cleanupKey(entry.getKey()));
            } else {
                newDenyList.add(cleanupKey(entry.getKey()));
            }
        }

        this.denyList = newDenyList;
        this.allowList = newAllowList;
    }

    private String cleanupKey(String key) {
        if (key.startsWith("")) {
            return key.substring(1, key.length() - 1);
        }

        return key;
    }

    private Optional<Boolean> isControlledViaAnnotation(Executable method) {
        return Optional.ofNullable(method.getAnnotation(NoodleSandbox.class))
                       .or(() -> Optional.ofNullable(method.getDeclaringClass().getAnnotation(NoodleSandbox.class)))
                       .map(Sandbox::isAccessGranted)
                       .or(() -> checkFieldForGetter(method));
    }

    private Optional<Boolean> checkFieldForGetter(Executable method) {
        try {
            if (method.getName().startsWith("get") || method.getName().startsWith("set")) {
                String fieldName = method.getName().substring(3);
                return Optional.ofNullable(method.getDeclaringClass()
                                                 .getDeclaredField(fieldName)
                                                 .getAnnotation(NoodleSandbox.class)).map(Sandbox::isAccessGranted);
            }
            if (method.getName().startsWith("is")) {
                String fieldName = method.getName().substring(2);
                return Optional.ofNullable(method.getDeclaringClass()
                                                 .getDeclaredField(fieldName)
                                                 .getAnnotation(NoodleSandbox.class)).map(Sandbox::isAccessGranted);
            }
        } catch (NoSuchFieldException e) {
            Exceptions.ignore(e);
        }

        return Optional.empty();
    }
}
