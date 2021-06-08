/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.transformers.Transformable;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.MethodPointer;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.TypeTools;
import sirius.pasta.noodle.sandbox.Sandbox;
import sirius.web.security.UserContext;

import javax.annotation.Nullable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Invokes a Java {@link Method}.
 */
public class MethodCall extends Call {

    private final String methodName;
    private Method method;
    private Node selfNode;

    @Part
    private static Sandbox sandbox;

    /**
     * Creates a new instance and specifies the expression on which the method is invoked.
     *
     * @param position   the position within the source code
     * @param self       the expression on which the method is invoked.
     * @param methodName the method to call
     */
    public MethodCall(Position position, Node self, String methodName) {
        super(position);
        this.selfNode = self;
        this.methodName = methodName;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        this.selfNode = selfNode.reduce(compilationContext);

        if (method != null) {
            Node intrinsic = optimizeIntrinsics();
            if (intrinsic != null) {
                return intrinsic;
            }
        }

        return super.reduce(compilationContext);
    }

    @SuppressWarnings({"java:S3776", "java:S1541"})
    @Explain("We rather keep all optimizations in one place.")
    private Node optimizeIntrinsics() {
        if (NLS.class.equals(method.getDeclaringClass()) && "get".equals(method.getName())) {
            return new IntrinsicCall(getPosition(),
                                     method.getGenericReturnType(),
                                     OpCode.INTRINSIC_NLS_GET,
                                     parameterNodes);
        }
        if (Strings.class.equals(method.getDeclaringClass())) {
            if ("isFilled".equals(method.getName())) {
                return new IntrinsicCall(getPosition(),
                                         method.getGenericReturnType(),
                                         OpCode.INTRINSIC_STRINGS_IS_FILLED,
                                         parameterNodes);
            }
            if ("isEmpty".equals(method.getName())) {
                return new IntrinsicCall(getPosition(),
                                         method.getGenericReturnType(),
                                         OpCode.INTRINSIC_STRINGS_IS_EMPTY,
                                         parameterNodes);
            }
        }
        if (Transformable.class.isAssignableFrom(selfNode.getType())) {
            if ("is".equals(method.getName())) {
                return new IntrinsicCall(getPosition(),
                                         boolean.class,
                                         OpCode.INTRINSIC_TRANSFORMABLE_IS,
                                         new Node[]{selfNode, parameterNodes[0]});
            }
            if ("as".equals(method.getName()) && parameterNodes[0].isConstant()) {
                return new IntrinsicCall(getPosition(),
                                         (Class<?>) parameterNodes[0].getConstantValue(),
                                         OpCode.INTRINSIC_TRANSFORMABLE_AS,
                                         new Node[]{selfNode, parameterNodes[0]});
            }
        }
        if (Value.class.equals(method.getDeclaringClass()) && "of".equals(method.getName())) {
            return new IntrinsicCall(getPosition(),
                                     method.getGenericReturnType(),
                                     OpCode.INTRINSIC_VALUE_OF,
                                     parameterNodes);
        }
        if (UserContext.class.equals(method.getDeclaringClass())) {
            if ("getCurrentUser".equals(method.getName())) {
                return new IntrinsicCall(getPosition(),
                                         method.getGenericReturnType(),
                                         OpCode.INTRINSIC_USER_CONTEXT_CURRENT_USER,
                                         parameterNodes);
            }
            if ("getHelper".equals(method.getName()) && parameterNodes.length == 1 && parameterNodes[0].isConstant()) {
                return new IntrinsicCall(getPosition(),
                                         (Class<?>) parameterNodes[0].getConstantValue(),
                                         OpCode.INTRINSIC_USER_CONTEXT_HELPER,
                                         parameterNodes);
            }
        }

        return null;
    }

    @Nullable
    @Override
    public Type getGenericType() {
        if (method == null) {
            return null;
        }

        TypeTools typeTools = new TypeTools(selfNode.getGenericType() == null ?
                                            selfNode.getType() :
                                            selfNode.getGenericType()).withMethod(method, parameterNodes);
        Type returnType = typeTools.simplify(method.getGenericReturnType());
        if (returnType instanceof TypeVariable) {
            return method.getReturnType();
        } else {
            return returnType;
        }
    }

    /**
     * Tries to find a matching method for the given name, type of "self" and parameter types.
     *
     * @param compilationContext the compilation context for error reporting
     * @return <tt>true</tt> if the method was bound successfully, <tt>false</tt> otherwise
     */
    @SuppressWarnings("ArrayEquality")
    @Explain("This is a re-used constant so an identity check works fine here")
    public boolean tryBindToMethod(CompilationContext compilationContext) {
        try {
            Class<?>[] parameterTypes = new Class<?>[parameterNodes.length];
            for (int i = 0; i < parameterNodes.length; i++) {
                parameterTypes[i] = parameterNodes[i].getType();
            }

            this.method = findMethod(selfNode.getType(), methodName, parameterTypes);
            checkStaticCallSite(compilationContext);
            checkDeprecation(compilationContext);
            checkSandbox(compilationContext);
        } catch (NoSuchMethodException e) {
            compilationContext.error(position, "%s doesn't have a method '%s'", selfNode.getType(), e.getMessage());
        }

        return method != null;
    }

    private void checkStaticCallSite(CompilationContext compilationContext) {
        if (selfNode instanceof RawClassLiteral && !Modifier.isStatic(method.getModifiers())) {
            compilationContext.error(position, "The called method '%s' isn't static.", method.getName());
        } else if (!(selfNode instanceof RawClassLiteral) && Modifier.isStatic(method.getModifiers())) {
            compilationContext.warning(position,
                                       "The called method '%s' is static and can be invoked without an object.",
                                       method.getName());
        }
    }

    private void checkDeprecation(CompilationContext context) {
        if (this.method.isAnnotationPresent(Deprecated.class)) {
            context.warning(position,
                            "The method %s.%s is marked as deprecated",
                            this.method.getDeclaringClass().getName(),
                            this.method.getName());
        }
    }

    private void checkSandbox(CompilationContext context) {
        if (context.isSandboxEnabled() && !sandbox.canInvoke(method)) {
            context.error(position,
                          "Cannot invoke %s on %s as this is restricted by the security sandbox!",
                          method.getName(),
                          method.getDeclaringClass().getName());
            this.method = null;
        }
    }

    private Method findMethod(Class<?> type, String name, Class<?>[] parameterTypes) throws NoSuchMethodException {
        // First try a regular lookup without coercing of any kind.
        // This is reasonable, as this takes generic type parameters
        // into account and selects the proper method...
        try {
            Method fastMatch = type.getMethod(name, parameterTypes);
            if (!fastMatch.isBridge()) {
                return fastMatch;
            }
        } catch (NoSuchMethodException e) {
            Exceptions.ignore(e);
        }

        // Try to find an appropriate method using coercions known to the system...
        for (Method candidateMethod : type.getMethods()) {
            if (!candidateMethod.isBridge() && signatureMatch(candidateMethod, name, parameterTypes)) {
                return candidateMethod;
            }
        }

        throw new NoSuchMethodException(name);
    }

    private boolean signatureMatch(Method method, String name, Class<?>[] parameterTypes) {
        if (!Strings.areEqual(name, method.getName())) {
            return false;
        }

        // If the method is a pure var arg method and no parameters are given at all.... this will work out...
        if (parameterTypes.length == 0 && method.getParameterCount() == 1 && method.getParameterTypes()[0].isArray()) {
            return true;
        }

        // Check all given parameters and pickup and check the var arg type if the method has one...
        if (!checkParameterTypes(method, parameterTypes)) {
            return false;
        }

        return ensureEnoughParameters(method, parameterTypes);
    }

    @SuppressWarnings("squid:S3776")
    @Explain("As this involves a stateful complex check, we rather keep all checks in one place.")
    private boolean checkParameterTypes(Method method, Class<?>[] parameterTypes) {
        Class<?> varargType = null;
        for (int i = 0; i < parameterTypes.length; i++) {
            Class<?> parameterType = parameterTypes[i];
            Class<?> methodParameterType = i < method.getParameterCount() ? method.getParameterTypes()[i] : varargType;

            // If the last parameter is an array, we have to determine its component type as this might be
            // a var-arg call...
            if (method.isVarArgs() && i == method.getParameterCount() - 1 && methodParameterType.isArray()) {
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
                    && parameterTypes.length > method.getParameterCount()) {
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

    private boolean ensureEnoughParameters(Method method, Class<?>[] parameterTypes) {
        // The method accepts all given parameters, now ensure, that we also provide enough parameters for the method...
        if (!method.isVarArgs()) {
            // No varargs -> parameters must match exactly...
            return method.getParameterTypes().length == parameterTypes.length;
        } else {
            // Varargs -> we can at most skip the last parameter...
            return parameterTypes.length >= method.getParameterTypes().length - 1;
        }
    }

    @Override
    public String toString() {
        return "MethodCall: " + selfNode + "." + methodName + "(" + Arrays.stream(parameterNodes)
                                                                          .map(Object::toString)
                                                                          .collect(Collectors.joining(", ")) + ")";
    }

    public Method getMethod() {
        return method;
    }

    public String getMethodName() {
        return methodName;
    }

    @Override
    public void emit(Assembler assembler) {
        try {
            if (method == null) {
                return;
            }

            for (int i = parameterNodes.length - 1; i >= 0; i--) {
                parameterNodes[i].emit(assembler);
            }

            boolean isStatic = Modifier.isStatic(method.getModifiers());
            if (!isStatic) {
                selfNode.emit(assembler);
            }

            assembler.emitPushConstant(new MethodPointer(method), position);
            assembler.emitByteCode(isStatic ? OpCode.INVOCE_STATIC : OpCode.INVOKE, parameterNodes.length, position);
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(Log.SYSTEM)
                            .error(e)
                            .withSystemErrorMessage("Failed to access method %s of %s: %s (%s)", methodName, selfNode)
                            .handle();
        }
    }
}
