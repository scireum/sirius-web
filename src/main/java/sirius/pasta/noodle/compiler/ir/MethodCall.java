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
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.sandbox.Sandbox;
import sirius.web.security.UserContext;

import javax.annotation.Nullable;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Invokes a Java {@link Method}.
 */
public class MethodCall extends Call {

    private String methodName;
    private Method method;
    private Node selfNode;

    @Part
    private static Sandbox sandbox;

    /**
     * Creates a new instance and specifies the expression on which the method is invoked.
     *
     * @param self       the expression on which the method is invoked.
     * @param methodName
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

    private Node optimizeIntrinsics() {
        if (NLS.class.equals(method.getDeclaringClass()) && "get".equals(method.getName())) {
            return new IntrinsicCall(getPosition(),
                                     method.getReturnType(),
                                     OpCode.OP_INTRINSIC_NLS_GET,
                                     parameterNodes);
        }
        if (Strings.class.equals(method.getDeclaringClass())) {
            if ("isFilled".equals(method.getName())) {
                return new IntrinsicCall(getPosition(),
                                         method.getReturnType(),
                                         OpCode.OP_INTRINSIC_STRINGS_IS_FILLED,
                                         parameterNodes);
            }
            if ("isEmpty".equals(method.getName())) {
                return new IntrinsicCall(getPosition(),
                                         method.getReturnType(),
                                         OpCode.OP_INTRINSIC_STRINGS_IS_EMPTY,
                                         parameterNodes);
            }
        }
        if (Transformable.class.isAssignableFrom(selfNode.getType())) {
            if ("is".equals(method.getName())) {
                return new IntrinsicCall(getPosition(),
                                         boolean.class,
                                         OpCode.OP_INTRINSIC_TRANSFORMABLE_IS,
                                         new Node[]{selfNode, parameterNodes[0]});
            }
            if ("as".equals(method.getName()) && parameterNodes[0].isConstant()) {
                return new IntrinsicCall(getPosition(),
                                         (Class<?>) parameterNodes[0].getConstantValue(),
                                         OpCode.OP_INTRINSIC_TRANSFORMABLE_AS,
                                         new Node[]{selfNode, parameterNodes[0]});
            }
        }
        if (Value.class.equals(method.getDeclaringClass()) && "of".equals(method.getName())) {
            return new IntrinsicCall(getPosition(),
                                     method.getReturnType(),
                                     OpCode.OP_INTRINSIC_VALUE_OF,
                                     parameterNodes);
        }
        if (UserContext.class.equals(method.getDeclaringClass())) {
            if ("getCurrentUser".equals(method.getName())) {
                return new IntrinsicCall(getPosition(),
                                         method.getReturnType(),
                                         OpCode.OP_INTRINSIC_USER_CONTEXT_CURRENT_USER,
                                         parameterNodes);
            }
            if ("getHelper".equals(method.getName()) && parameterNodes.length == 1 && parameterNodes[0].isConstant()) {
                return new IntrinsicCall(getPosition(),
                                         (Class<?>) parameterNodes[0].getConstantValue(),
                                         OpCode.OP_INTRINSIC_USER_CONTEXT_HELPER,
                                         parameterNodes);
            }
        }

        return null;
    }

    @Override
    public Class<?> getType() {
        if (method == null) {
            return void.class;
        }

        // Try to resolve type parameters into their actual values if possible.
        // This will propagate type parameters down a call chain.
        Type genericType = getGenericType();
        if (genericType instanceof Class<?>) {
            return (Class<?>) genericType;
        }
        if (genericType instanceof ParameterizedType) {
            return (Class<?>) ((ParameterizedType) genericType).getRawType();
        }

        return method.getReturnType();
    }

    @Nullable
    @Override
    public Type getGenericType() {
        if (method == null) {
            return null;
        }

        if (method.getGenericReturnType() instanceof TypeVariable) {
            Type parameterizedType = tryResolveViaParameterizedCallee();
            if (parameterizedType != null) {
                return parameterizedType;
            }

            Type constantClassType = tryResolveViaConstantClassParameter();
            if (constantClassType != null) {
                return constantClassType;
            }
        }

        return method.getGenericReturnType();
    }

    /**
     * Tries to resolve a type variable by inspecting the parameterization of the callsite.
     * <p>
     * If a method returns a generic variable like {@code ValueHolder<V>.get()} does,
     * we try to determine the generic type parameters from the class we're being invoked
     * on. If we end up at a method or variable declaration which specifies the types, we
     * can propagate this information down a call chain and therefore deduce the effective type.
     *
     * @return the type deduced from a parameterization of the "this" expression or <tt>null</tt>
     * if not enough information is present
     */
    private Type tryResolveViaParameterizedCallee() {
        Type parentType = selfNode.getGenericType();
        if (parentType instanceof ParameterizedType) {
            int index = determineGenericIndex(method.getGenericReturnType().getTypeName(),
                                              (Class<?>) ((ParameterizedType) parentType).getRawType());
            if (index >= 0) {
                return ((ParameterizedType) parentType).getActualTypeArguments()[index];
            }
        }

        return null;
    }

    /**
     * Tries to deduce the return type of a method by looking for appropriate class parameters.
     * <p>
     * Many methods return an object of a given class like {@link sirius.web.security.UserContext#getHelper(Class)}
     * does. Therefore, if we detect a type variable as return type, we try to find a constant class parameter which
     * binds to the same type variable and try to deduce the effective return type this way.
     *
     * @return the deduced class parameter for the returned type variable or <tt>null</tt> if not enough information
     * if present
     */
    private Type tryResolveViaConstantClassParameter() {
        String variableName = ((TypeVariable<?>) method.getGenericReturnType()).getName();
        for (int i = 0; i < method.getParameterCount(); i++) {
            Type resolvedType = tryResolveAsConstantClassParameter(variableName, i);
            if (resolvedType != null) {
                return resolvedType;
            }
        }

        return null;
    }

    private Type tryResolveAsConstantClassParameter(String variableName, int parameterIndex) {
        // Ensure that the parameter is of type Class<X> - abort otherwise
        if (!(Class.class.equals(method.getParameterTypes()[parameterIndex])
              && method.getGenericParameterTypes()[parameterIndex] instanceof ParameterizedType)) {
            return null;
        }

        // Read the actual type argument, this would be the X of Class<X>
        Type actualTypeArgument =
                ((ParameterizedType) method.getGenericParameterTypes()[parameterIndex]).getActualTypeArguments()[0];

        // Abort if this isn't a type variable...
        if (!(actualTypeArgument instanceof TypeVariable)) {
            return null;
        }

        String typeVariableName = ((TypeVariable<?>) actualTypeArgument).getName();
        // Make sure that this class actually binds to our type variable and not to
        // anything else...
        if (!Strings.areEqual(variableName, typeVariableName)) {
            return null;
        }

        // For a constant class, we know the resulting type...
        if (parameterNodes[parameterIndex].isConstant()
            && Class.class.isAssignableFrom(parameterNodes[parameterIndex].getType())) {
            return (Class<?>) parameterNodes[parameterIndex].getConstantValue();
        }

        // Otherwise resort to the generic type - if available...
        Type genericType = parameterNodes[parameterIndex].getGenericType();
        if (genericType != null) {
            return genericType;
        }

        // Or finally resort to the known type...
        return parameterNodes[parameterIndex].getType();
    }

    private int determineGenericIndex(String parameterName, Class<?> clazz) {
        int index = 0;
        for (TypeVariable<?> param : clazz.getTypeParameters()) {
            if (param.getName().equals(parameterName)) {
                return index;
            }
            index++;
        }

        return -1;
    }

    /**
     * Tries to find a matching method for the given name, type of "self" and parameter types.
     *
     * @param position the position where the invocation was declared
     * @param context  the compilation context for error reporting
     * @param name     the name of the method to find
     */
    public boolean tryBindToMethod(CompilationContext compilationContext) {
        if (parameterNodes == NO_ARGS) {
            try {
                this.method = selfNode.getType().getMethod(methodName);
                checkDeprecation(compilationContext);
                checkStaticCallSite(compilationContext);
                checkSandbox(compilationContext);
                return true;
            } catch (NoSuchMethodException e) {
                Exceptions.ignore(e);
            }
        }

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
            return type.getMethod(name, parameterTypes);
        } catch (NoSuchMethodException e) {
            Exceptions.ignore(e);
        }

        // Try to find an appropriate method using coercions known to the system...
        for (Method m : type.getMethods()) {
            if (signatureMatch(m, name, parameterTypes)) {
                return m;
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
        return "MethdoCall: " + selfNode + "." + methodName + "(" + Arrays.stream(parameterNodes)
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

            MethodHandles.Lookup lookup = MethodHandles.lookup();
            MethodHandle handle = lookup.unreflect(method);

            for (int i = parameterNodes.length - 1; i >= 0; i--) {
                parameterNodes[i].emit(assembler);
            }

            boolean isStatic = selfNode instanceof RawClassLiteral;
            if (!isStatic) {
                selfNode.emit(assembler);
            }

            assembler.emitPushConstant(handle, position);
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