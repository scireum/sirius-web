/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import java.lang.reflect.Type;

/**
 * Represents an optimized call which simply returns a shared variable.
 * <p>
 * Calls like these are quite common in <tt>Tagliatelle</tt> as templates often output arguments or
 * other variables.
 */
public class ReturnVariableCall implements Callable {

    private final String variableName;
    private final int variableIndex;
    private final Class<?> returnType;
    private final Type genericReturnType;

    /**
     * Creates a new call for the given variable.
     *
     * @param variableName      the name of the variable
     * @param variableIndex     the index within the environment
     * @param returnType        the type of the variable
     * @param genericReturnType the generic type of the variable
     */
    public ReturnVariableCall(String variableName, int variableIndex, Class<?> returnType, Type genericReturnType) {
        this.variableName = variableName;
        this.variableIndex = variableIndex;
        this.returnType = returnType;
        this.genericReturnType = genericReturnType;
    }

    @Override
    public Class<?> getType() {
        return returnType;
    }

    @Override
    public Type getGenericType() {
        return genericReturnType;
    }

    @Override
    public Object call(Environment environment) {
        return environment.readVariable(variableIndex);
    }

    @Override
    public String toString() {
        return "ReturnVariableCall: " + variableName;
    }
}
