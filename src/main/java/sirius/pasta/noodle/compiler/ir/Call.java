/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.pasta.noodle.compiler.CompilationContext;

import java.lang.reflect.Executable;
import java.util.List;

/**
 * Represents an invocation with parameters.
 */
public abstract class Call extends Node {

    /**
     * A placeholder to represent "no arguments", which is preferred over <tt>null</tt> or creating an empty array each
     * time.
     */
    public static final Node[] NO_ARGS = {};
    protected Node[] parameterNodes = NO_ARGS;

    protected Call(Position position) {
        super(position);
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        for (int i = 0; i < parameterNodes.length; i++) {
            parameterNodes[i] = parameterNodes[i].reduce(compilationContext);
        }

        return this;
    }

    /**
     * Applies the parameters to evaluate and pass the to invocation.
     *
     * @param parameters the parameters to apply
     */
    public void setParameters(List<Node> parameters) {
        if (parameters != null && !parameters.isEmpty()) {
            this.parameterNodes = parameters.toArray(NO_ARGS);
        }
    }

    /**
     * Permits to access the n-th parameter.
     *
     * @param index the parameter to access
     * @return the node which is passed in as parameter
     */
    public Node getParameter(int index) {
        return parameterNodes[index];
    }


    protected boolean signatureMatch(Executable executable, String name, Class<?>[] parameterTypes) {
        if (name != null && !Strings.areEqual(executable.getName(), name)) {
            return false;
        }

        // If the method is a pure var arg method and no parameters are given at all.... this will work out...
        if (parameterTypes.length == 0
            && executable.getParameterCount() == 1
            && executable.getParameterTypes()[0].isArray()) {
            return true;
        }

        // Check all given parameters and pickup and check the var arg type if the method has one...
        if (!checkParameterTypes(executable, parameterTypes)) {
            return false;
        }

        return ensureEnoughParameters(executable, parameterTypes);
    }

    @SuppressWarnings("squid:S3776")
    @Explain("As this involves a stateful complex check, we rather keep all checks in one place.")
    protected boolean checkParameterTypes(Executable executable, Class<?>[] parameterTypes) {
        Class<?> varargType = null;
        for (int i = 0; i < parameterTypes.length; i++) {
            Class<?> parameterType = parameterTypes[i];
            Class<?> methodParameterType =
                    i < executable.getParameterCount() ? executable.getParameterTypes()[i] : varargType;

            // If the last parameter is an array, we have to determine its component type as this might be
            // a var-arg call...
            if (executable.isVarArgs() && i == executable.getParameterCount() - 1 && methodParameterType.isArray()) {
                varargType = methodParameterType.getComponentType();

                // For a var-arg parameter, the last parameter has either to be the exact same type (an array)
                // or it has to match the vararg type
                if (!CompilationContext.isAssignableTo(parameterType, methodParameterType)
                    && !CompilationContext.isAssignableTo(parameterType, varargType)) {
                    return false;
                }

                // If we matched the original array type, we cannot support additional vararg parameters,
                // therefore the argument count must match...
                if (!CompilationContext.isAssignableTo(parameterType, varargType)
                    && parameterTypes.length > executable.getParameterCount()) {
                    return false;
                }
            } else if (methodParameterType == null || !CompilationContext.isAssignableTo(parameterType,
                                                                                         methodParameterType)) {
                // For all other parameters (than the last) we simply ensure, that the parameter type is assignable...
                return false;
            }
        }

        return true;
    }

    protected boolean ensureEnoughParameters(Executable executable, Class<?>[] parameterTypes) {
        // The method accepts all given parameters, now ensure, that we also provide enough parameters for the method...
        if (!executable.isVarArgs()) {
            // No varargs -> parameters must match exactly...
            return executable.getParameterTypes().length == parameterTypes.length;
        } else {
            // Varargs -> we can at most skip the last parameter...
            return parameterTypes.length >= executable.getParameterTypes().length - 1;
        }
    }

}
