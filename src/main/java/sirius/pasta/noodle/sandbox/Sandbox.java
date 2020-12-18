/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;

import sirius.kernel.Sirius;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Provides a security sandbox in case <tt>Noodle</tt> is compiling a script or template provided by a user.
 * <p>
 * User code may only access methods, macros or fields which have been whitelisted. This can either be
 * performed by placing a {@link PublicAPI} annotation or by adding an entry in the system config in
 * <tt>scripting.sandbox</tt>.
 * <p>
 * Note that the sandbox is applied at compile time and therefore has no runtime overhead at all. Also note that
 * common scripts and templates (provided by the application or an system administrator) run without a sandbox
 * and therefore have full access to the whole JVM.
 */
@Register(classes = Sandbox.class)
public class Sandbox {

    private Set<String> denylist;
    private Set<String> allowlist;

    /**
     * Determines if the given method can be invoked by user code.
     *
     * @param method the method to check
     * @return <tt>true</tt> if it can be invoked, <tt>false</tt> otherwise
     */
    public boolean canInvoke(Method method) {
        if (isAllowedPerAnnotation(method)) {
            return true;
        }

        return isAllowed(method.getDeclaringClass().getName(), method.getName());
    }

    private boolean isAllowed(String className, String methodName) {
        if (denylist == null) {
            loadConfig();
        }
        return !denylist.contains(className + "." + methodName) && (allowlist.contains(className + "." + methodName)
                                                                    || allowlist.contains(className + ".*"));
    }

    private void loadConfig() {
        Set<String> newDenylist = new HashSet<>();
        Set<String> newAllowlist = new HashSet<>();

        for (Map.Entry<String, String> entry : Sirius.getSettings().getMap("scripting.sandbox").entrySet()) {
            if (Boolean.parseBoolean(entry.getValue())) {
                newAllowlist.add(entry.getKey());
            } else {
                newDenylist.add(entry.getKey());
            }
        }

        this.denylist = newDenylist;
        this.allowlist = newAllowlist;
    }

    private boolean isAllowedPerAnnotation(Method method) {
        if (method.isAnnotationPresent(PublicAPI.class)) {
            return true;
        }
        if (method.getDeclaringClass().isAnnotationPresent(PublicAPI.class)) {
            return true;
        }

        return checkFieldForGetter(method);
    }

    private boolean checkFieldForGetter(Method method) {
        try {
            if (method.getName().startsWith("get") || method.getName().startsWith("set")) {
                String fieldName = method.getName().substring(3, 3).toLowerCase() + method.getName().substring(4);
                return method.getDeclaringClass().getDeclaredField(fieldName).isAnnotationPresent(PublicAPI.class);
            }
            if (method.getName().startsWith("is")) {
                String fieldName = method.getName().substring(2, 2).toLowerCase() + method.getName().substring(3);
                return method.getDeclaringClass().getDeclaredField(fieldName).isAnnotationPresent(PublicAPI.class);
            }
        } catch (NoSuchFieldException e) {
            Exceptions.ignore(e);
        }

        return false;
    }
}
