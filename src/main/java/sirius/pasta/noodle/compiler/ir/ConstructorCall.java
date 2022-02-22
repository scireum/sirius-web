/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.TypeTools;
import sirius.pasta.noodle.sandbox.Sandbox;

import javax.annotation.Nullable;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Invokes a Java {@link Constructor}.
 */
public class ConstructorCall extends Call {

    private Constructor<?> constructor;
    private final RawClassLiteral selfNode;

    @Part
    private static Sandbox sandbox;

    /**
     * Creates a new instance and specifies the expression on which the constructor is invoked.
     *
     * @param position the position within the source code
     * @param self     the class to construct
     */
    public ConstructorCall(Position position, RawClassLiteral self) {
        super(position);
        this.selfNode = self;
    }

    @Nullable
    @Override
    public Type getGenericType() {
        if (constructor == null) {
            return null;
        }

        TypeTools typeTools = new TypeTools(selfNode.getGenericType()).withMethod(constructor, parameterNodes);
        Type returnType = typeTools.simplify(constructor.getAnnotatedReturnType().getType());
        if (returnType instanceof TypeVariable) {
            return constructor.getDeclaringClass();
        } else {
            return returnType;
        }
    }

    /**
     * Tries to find a matching constructor for type of "self" and parameter types.
     *
     * @param compilationContext the compilation context for error reporting
     * @return <tt>true</tt> if the constructor was bound successfully, <tt>false</tt> otherwise
     */
    public boolean tryBindToConstructor(CompilationContext compilationContext) {
        try {
            Class<?>[] parameterTypes = new Class<?>[parameterNodes.length];
            for (int i = 0; i < parameterNodes.length; i++) {
                parameterTypes[i] = parameterNodes[i].getType();
            }

            this.constructor = findConstructor((Class<?>) selfNode.getConstantValue(), parameterTypes);
            checkDeprecation(compilationContext);
            checkSandbox(compilationContext);
        } catch (NoSuchMethodException e) {
            compilationContext.error(position,
                                     "%s doesn't have a constructor '%s'",
                                     selfNode.getType(),
                                     e.getMessage());
        }

        return constructor != null;
    }

    private void checkDeprecation(CompilationContext context) {
        if (this.constructor.isAnnotationPresent(Deprecated.class)) {
            context.warning(position,
                            "The method %s.%s is marked as deprecated",
                            this.constructor.getDeclaringClass().getName(),
                            this.constructor.getName());
        }
    }

    private void checkSandbox(CompilationContext context) {
        if (context.isSandboxEnabled() && !sandbox.canInvoke(constructor)) {
            context.error(position,
                          "Cannot invoke the constructor on %s as this is restricted by the security sandbox!",
                          constructor.getDeclaringClass().getName());
            this.constructor = null;
        }
    }

    private Constructor<?> findConstructor(Class<?> type, Class<?>[] parameterTypes) throws NoSuchMethodException {
        // First try a regular lookup without coercing of any kind.
        // This is reasonable, as this takes generic type parameters
        // into account and selects the proper method...
        try {
            return type.getConstructor(parameterTypes);
        } catch (NoSuchMethodException e) {
            Exceptions.ignore(e);
        }

        // Try to find an appropriate method using coercions known to the system...
        for (Constructor<?> candidateConstructor : type.getConstructors()) {
            if (signatureMatch(candidateConstructor, null, parameterTypes)) {
                return candidateConstructor;
            }
        }

        throw new NoSuchMethodException("new");
    }

    @Override
    public String toString() {
        return "ConstructorCall: " + selfNode + ".new(" + Arrays.stream(parameterNodes)
                                                                .map(Object::toString)
                                                                .collect(Collectors.joining(", ")) + ")";
    }

    public Constructor<?> getConstructor() {
        return constructor;
    }

    @Override
    public void emit(Assembler assembler) {
        try {
            if (constructor == null) {
                return;
            }

            for (int i = parameterNodes.length - 1; i >= 0; i--) {
                parameterNodes[i].emit(assembler);
            }

            assembler.emitPushConstant(constructor, position);
            assembler.emitByteCode(OpCode.INVOCE_STATIC, parameterNodes.length, position);
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(Log.SYSTEM)
                            .error(e)
                            .withSystemErrorMessage("Failed to access constructor of %s: %s (%s)", selfNode)
                            .handle();
        }
    }
}
