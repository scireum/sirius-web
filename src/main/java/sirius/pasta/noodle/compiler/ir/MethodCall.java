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
