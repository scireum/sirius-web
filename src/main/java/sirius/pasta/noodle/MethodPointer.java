/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.util.Objects;

/**
 * Wraps a {@link Method} along with its {@link MethodHandle} together.
 * <p>
 * This helps to provide a sane "toString" method and also supports calling "equals" - in contrast to
 * <tt>MethodHandle</tt>.
 */
public class MethodPointer {

    private final Method method;
    private final MethodHandle methodHandle;

    /**
     * Creates a new pointer for the given method.
     *
     * @param method the method to point to
     * @throws IllegalAccessException in case the method isn't accessible
     */
    public MethodPointer(Method method) throws IllegalAccessException {
        this.method = method;
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        this.methodHandle = lookup.unreflect(method);
    }

    public Method getMethod() {
        return method;
    }

    public MethodHandle getMethodHandle() {
        return methodHandle;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        MethodPointer that = (MethodPointer) o;
        return method.equals(that.method);
    }

    @Override
    public int hashCode() {
        return Objects.hash(method);
    }

    @Override
    public String toString() {
        return method.toString();
    }
}
