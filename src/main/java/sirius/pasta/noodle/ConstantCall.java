/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import sirius.kernel.nls.NLS;

import java.lang.reflect.Type;

/**
 * Represents a script which simply returns a constant value.
 */
public class ConstantCall implements Callable {

    /**
     * Contains a shared instance which constantly returns <tt>null</tt>.
     */
    public static final ConstantCall NULL = new ConstantCall(null);

    /**
     * Contains a shared instance which constantly returns an empty string.
     */
    public static final Callable EMPTY_STRING = new ConstantCall("");

    private final Object value;

    /**
     * Creates a new constant call which always yields the given value.
     *
     * @param value the value to return
     */
    public ConstantCall(Object value) {
        this.value = value;
    }

    /**
     * Determines if the given call will constantly return <tt>null</tt>.
     *
     * @param call the call to inspect
     * @return <tt>true</tt> if a compile time analysis determines that the given call will constantly return
     * <tt>null</tt>, <tt>false</tt> otherwise
     */
    public static boolean isConstantNull(Callable call) {
        return (call instanceof ConstantCall constantCall) && constantCall.value == null;
    }

    @Override
    public Class<?> getType() {
        if (value == null) {
            return void.class;
        }

        return value.getClass();
    }

    @Override
    public Type getGenericType() {
        return getType();
    }

    @Override
    public Object call(Environment environment) {
        return value;
    }

    @Override
    public String toString() {
        return "ConstantCall: " + NLS.toMachineString(value);
    }
}
