/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.transformers.Transformable;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.pasta.Pasta;
import sirius.pasta.noodle.compiler.ir.LambdaNode;
import sirius.pasta.noodle.macros.Macro;
import sirius.web.security.UserContext;

import java.lang.invoke.MethodHandle;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Represents the actual interpreter which executes an {@link InterpreterCall} within an {@link Environment}.
 */
public class Invocation {

    private static final Object[] NO_ARGS = new Object[0];

    /**
     * Contains commonly shared constants to minimize the constant pool of each {@link InterpreterCall}.
     */
    public static final SharedConstantPool SHARED_CONSTANT_POOL = new SharedConstantPool();

    /**
     * Provides access to the script to execute
     */
    protected InterpreterCall compiledMethod;

    /**
     * Represents the local stack used by the VM.
     */
    protected List<Object> stack = new ArrayList<>();

    /**
     * Contains the environment used to access shared variables.
     */
    protected Environment environment;

    /**
     * Contains the "instruction pointer" which contains the index of the bytecode to execute next.
     */
    protected int instructionPointer;

    /**
     * Creates a new interpreter for the given script and environment.
     *
     * @param method      the script to execute
     * @param environment the environment to use
     */
    public Invocation(InterpreterCall method, Environment environment) {
        this(method, environment, 0);
    }

    /**
     * Creates a new interpreter for the given script and environment.
     *
     * @param method      the script to execute
     * @param environment the environment to use
     * @param initialIP   the first instruction to execute
     */
    public Invocation(InterpreterCall method, Environment environment, int initialIP) {
        this.compiledMethod = method;
        this.environment = environment;
        this.instructionPointer = initialIP;
    }

    /**
     * Actually runs the interpreter on the given bytecodes.
     * <p>
     * Note that this method is quite a beast, however it is kept together so that the whole logic is in one place.
     *
     * @return the result as yielded by the interpreter
     * @throws ScriptingException in case of an error within the script. See {@link ScriptingException} for a detailed
     *                            explanation of when this exception is used.
     */
    public Object execute() throws ScriptingException {
        try {
            return executeBytecodes();
        } catch (HandledException ex) {
            throw ex;
        } catch (Throwable ex) {
            Position position = compiledMethod.ipToPositionTable.get(instructionPointer - 1);
            throw new ScriptingException(Strings.apply(
                    "An error occurred while executing a script. Location: %s.%s%s (%s)",
                    compiledMethod.sourceCodeInfo.getName() + ":" + position.getLine() + ":" + position.getPos(),
                    computeOffendingPosition(position),
                    ex.getMessage(),
                    ex.getClass().getName()), ex);
        }
    }

    @SuppressWarnings({"unchecked", "java:S1541", "java:S134", "java:S1764", "OverlyLongMethod"})
    @Explain(
            "This method and also this switch statement is too large. However, we'd like to keep everything in one place if possible.")
    private Object executeBytecodes() throws Throwable {
        while (instructionPointer < compiledMethod.opcodes.size()) {
            int instruction = compiledMethod.opcodes.get(instructionPointer++);
            OpCode opCode = OpCode.values()[(instruction & 0x00FF0000) >> 16];
            int index = instruction & 0x0000FFFF;

            switch (opCode) {
                case PUSH_CONST:
                    push(compiledMethod.constants.get(index));
                    break;
                case PUSH_VARIABLE:
                    push(environment.readVariable(index));
                    break;
                case PUSH_BUILT_IN:
                    push(SHARED_CONSTANT_POOL.fetch(index));
                    break;
                case PUSH_FIELD:
                    handlePushField();
                    break;
                case POP_FIELD:
                    handlePopField();
                    break;
                case POP_VARIABLE:
                    environment.writeVariable(index, pop());
                    break;
                case POP_TOP:
                    pop();
                    break;
                case JMP_FALSE:
                    handleJumpFalse(index);
                    break;
                case JMP:
                    instructionPointer += index;
                    break;
                case JMP_BACK:
                    instructionPointer -= index;
                    break;
                case INVOKE:
                    handleInvoke(index, false);
                    break;
                case INVOCE_STATIC:
                    handleInvoke(index, true);
                    break;
                case LAMBDA:
                    handleLambda(index);
                    break;
                case RET_STACK_TOP:
                    return pop();
                case OP_EQ:
                    push(Objects.equals(pop(), pop()));
                    break;
                case OP_ID:
                    push(pop() == pop());
                    break;
                case OP_NE:
                    push(!Objects.equals(pop(), pop()));
                    break;
                case OP_GT:
                    push(pop(Comparable.class).compareTo(pop()) > 0);
                    break;
                case OP_GE:
                    push(pop(Comparable.class).compareTo(pop()) >= 0);
                    break;
                case OP_LT:
                    push(pop(Comparable.class).compareTo(pop()) < 0);
                    break;
                case OP_LE:
                    push(pop(Comparable.class).compareTo(pop()) <= 0);
                    break;
                case OP_NOT:
                    push(!pop(boolean.class));
                    break;
                case OP_ADD:
                    push(add(pop(), pop()));
                    break;
                case OP_SUB:
                    push(sub(pop(), pop()));
                    break;
                case OP_MUL:
                    push(mul(pop(), pop()));
                    break;
                case OP_DIV:
                    push(div(pop(), pop()));
                    break;
                case OP_MOD:
                    push(mod(pop(), pop()));
                    break;
                case OP_CONCAT:
                    push(asString(pop()) + asString(pop()));
                    break;
                case OP_CAST:
                    handleCast();
                    break;
                case OP_INSTANCE_OF:
                    handleInstanceOf();
                    break;
                case COERCE_INT_TO_LONG:
                    push(Long.valueOf(pop(int.class)));
                    break;
                case COERCE_INT_TO_DOUBLE:
                    push(Double.valueOf(pop(int.class)));
                    break;
                case COERCE_LONG_TO_DOUBLE:
                    push(Double.valueOf(pop(long.class)));
                    break;
                case INTRINSIC_TRANSFORMABLE_AS:
                    handleTransformableAs();
                    break;
                case INTRINSIC_TRANSFORMABLE_IS:
                    handleTransformableIs();
                    break;
                case INTRINSIC_STRINGS_IS_EMPTY:
                    push(Strings.isEmpty(pop()));
                    break;
                case INTRINSIC_STRINGS_IS_FILLED:
                    push(Strings.isFilled(pop()));
                    break;
                case INTRINSIC_VALUE_OF:
                    push(Value.of(pop()));
                    break;
                case INTRINSIC_NLS_GET:
                    push(NLS.get(pop(String.class)));
                    break;
                case INTRINSIC_USER_CONTEXT_HELPER:
                    push(UserContext.getHelper(pop(Class.class)));
                    break;
                case INTRINSIC_USER_CONTEXT_CURRENT_USER:
                    push(UserContext.getCurrentUser());
                    break;
                default:
                    throw new UnsupportedOperationException(String.valueOf(opCode));
            }
        }

        if (stack.isEmpty()) {
            return null;
        } else {
            return pop();
        }
    }

    private void handlePushField() {
        Field field = pop(Field.class);
        try {
            if (Modifier.isStatic(field.getModifiers())) {
                push(field.get(null));
            } else {
                push(field.get(pop()));
            }
        } catch (Exception e) {
            throw createVmError(Strings.apply("Cannot read the field %s of %s",
                                              field.getName(),
                                              field.getDeclaringClass().getName()));
        }
    }

    private void handlePopField() {
        Field field = pop(Field.class);
        try {
            if (Modifier.isStatic(field.getModifiers())) {
                field.set(null, pop());
            } else {
                Object self = pop();
                field.set(self, pop());
            }
        } catch (Exception e) {
            throw createVmError(Strings.apply("Cannot store into the field %s of %s",
                                              field.getName(),
                                              field.getDeclaringClass().getName()));
        }
    }

    /**
     * Handles {@link OpCode#LAMBDA}.
     * <p>
     * Note that the theory of operation is described in {@link LambdaNode}.
     *
     * @param numLocals the number of locals of the lambda
     */
    private void handleLambda(int numLocals) {
        int initialIP = instructionPointer + 1;
        int contextOffset = pop(int.class);
        Class<?> samInterface = pop(Class.class);
        Environment lambdaEnvironment = LambdaEnvironment.create(environment, contextOffset, numLocals);
        push(Proxy.newProxyInstance(getClass().getClassLoader(),
                                    new Class[]{samInterface},
                                    new LambdaHandler(initialIP,
                                                      contextOffset,
                                                      numLocals,
                                                      compiledMethod,
                                                      lambdaEnvironment)));
    }

    private void handleJumpFalse(int index) {
        Object value = pop();
        if (value == null || Boolean.FALSE.equals(value)) {
            instructionPointer += index;
        }
    }

    private void handleInstanceOf() {
        Class<?> type = pop(Class.class);
        Object self = pop();
        if (self != null) {
            push(type.isAssignableFrom(self.getClass()));
        } else {
            push(false);
        }
    }

    private void handleCast() {
        Class<?> type = pop(Class.class);
        Object self = pop();
        if (self != null) {
            push(type.cast(self));
        } else {
            push(null);
        }
    }

    private void handleTransformableIs() {
        Object self = pop();
        if (self instanceof Transformable) {
            push(((Transformable) self).is(pop(Class.class)));
        } else {
            push(false);
        }
    }

    @SuppressWarnings("unchecked")
    private void handleTransformableAs() {
        Object self = pop();
        if (self instanceof Transformable) {
            push(((Transformable) self).as(pop(Class.class)));
        } else {
            push(null);
        }
    }

    private String computeOffendingPosition(Position position) {
        String offendingLine = compiledMethod.sourceCodeInfo.fetchLine(position.getLine());
        if (Strings.isEmpty(offendingLine)) {
            return "\n";
        }

        StringBuilder result = new StringBuilder(" Line:\n");
        result.append(offendingLine);
        result.append("\n");
        result.append(" ".repeat(Math.max(0, position.getPos() - 2)));
        result.append("^\n");
        return result.toString();
    }

    /**
     * Creates an exception which is used when Noodle detects an inconsistent or invalid internal state.
     * <p>
     * These states should normally not be reached as the compiler should only emit valid bytecodes. Therefore
     * we provide quite some excessive logging here.
     *
     * @param message the message to log
     * @return an exception with a short and concise message of what happened. Also a long and elaborate message will
     * be logged which might provide further insight.
     */
    private IllegalArgumentException createVmError(String message) {
        // Create a full blown exception for the logs...
        IllegalStateException internalError = new IllegalStateException(Strings.apply(
                "A Noodle VM error occurred: %s%n%nIP: %s%nStack: %s%nCall:%n%n%s",
                message,
                instructionPointer,
                stack,
                this.compiledMethod.disassemble()));
        Exceptions.handle(Pasta.LOG, internalError);

        // Return a shortened exception which may even be delivered to the frontend...
        return new IllegalArgumentException(message);
    }

    private Object add(Object a, Object b) {
        if (a instanceof Integer) {
            return ((int) a) + (int) b;
        }
        if (a instanceof Long) {
            return ((long) a) + (long) b;
        }
        if (a instanceof Double) {
            return ((double) a) + (double) b;
        }

        throw createVmError(Strings.apply("Cannot add %s and %s (%s, %s)", a, b, a.getClass(), b.getClass()));
    }

    private Object sub(Object a, Object b) {
        if (a instanceof Integer) {
            return ((int) a) - (int) b;
        }
        if (a instanceof Long) {
            return ((long) a) - (long) b;
        }
        if (a instanceof Double) {
            return ((double) a) - (double) b;
        }

        throw createVmError(Strings.apply("Cannot subtract %s minus %s (%s, %s)", a, b, a.getClass(), b.getClass()));
    }

    private Object mul(Object a, Object b) {
        if (a instanceof Integer) {
            return ((int) a) * (int) b;
        }
        if (a instanceof Long) {
            return ((long) a) * (long) b;
        }
        if (a instanceof Double) {
            return ((double) a) * (double) b;
        }

        throw createVmError(Strings.apply("Cannot multiply %s by %s (%s, %s)", a, b, a.getClass(), b.getClass()));
    }

    private Object div(Object a, Object b) {
        if (a instanceof Integer) {
            return ((int) a) / (int) b;
        }
        if (a instanceof Long) {
            return ((long) a) / (long) b;
        }
        if (a instanceof Double) {
            return ((double) a) / (double) b;
        }

        throw createVmError(Strings.apply("Cannot divide %s by %s (%s, %s)", a, b, a.getClass(), b.getClass()));
    }

    private Object mod(Object a, Object b) {
        if (a instanceof Integer) {
            return ((int) a) % (int) b;
        }
        if (a instanceof Long) {
            return ((long) a) % (long) b;
        }
        if (a instanceof Double) {
            return ((double) a) % (double) b;
        }

        throw createVmError(Strings.apply("Cannot compute the modulo of %s and %s (%s, %s)",
                                          a,
                                          b,
                                          a.getClass(),
                                          b.getClass()));
    }

    private String asString(Object value) {
        return value == null ? "" : value.toString();
    }

    private void handleInvoke(int numberOfArguments, boolean isStatic) throws Throwable {
        Object target = pop();
        if (target instanceof MethodPointer methodPointer) {
            invokeMethod(numberOfArguments, isStatic, methodPointer.getMethodHandle());
        } else if (target instanceof Macro macro) {
            invokeMacro(numberOfArguments, macro);
        } else if (target instanceof Constructor<?> constructor) {
            invokeConstructor(numberOfArguments,constructor);
        } else {
            throw createVmError(Strings.apply("Cannot invoke: %s", target));
        }
    }

    private void invokeMethod(int numberOfArguments, boolean isStatic, MethodHandle methodHandle) throws Throwable {
        if (methodHandle.isVarargsCollector() || numberOfArguments > 3) {
            invokeMethodWithArguments(numberOfArguments, isStatic, methodHandle);
        } else if (isStatic) {
            invokeStaticMethod(numberOfArguments, methodHandle);
        } else {
            invokeMethod(numberOfArguments, methodHandle);
        }
    }

    private void invokeMethodWithArguments(int numberOfArguments, boolean isStatic, MethodHandle methodHandle)
            throws Throwable {
        Object[] args = popArguments(numberOfArguments, isStatic);
        push(methodHandle.invokeWithArguments(args));
    }

    private void invokeStaticMethod(int numberOfArguments, MethodHandle methodHandle) throws Throwable {
        if (numberOfArguments == 0) {
            push(methodHandle.invoke());
        } else if (numberOfArguments == 1) {
            push(methodHandle.invoke(pop()));
        } else if (numberOfArguments == 2) {
            Object arg1 = pop();
            Object arg2 = pop();
            push(methodHandle.invoke(arg1, arg2));
        } else if (numberOfArguments == 3) {
            Object arg1 = pop();
            Object arg2 = pop();
            Object arg3 = pop();
            push(methodHandle.invoke(arg1, arg2, arg3));
        }
    }

    private void invokeMethod(int numberOfArguments, MethodHandle methodHandle) throws Throwable {
        Object self = pop();
        if (self == null) {
            while (numberOfArguments-- > 0) {
                pop();
            }
            push(null);
        } else if (numberOfArguments == 0) {
            push(methodHandle.invoke(self));
        } else if (numberOfArguments == 1) {
            Object arg1 = pop();
            push(methodHandle.invoke(self, arg1));
        } else if (numberOfArguments == 2) {
            Object arg1 = pop();
            Object arg2 = pop();
            push(methodHandle.invoke(self, arg1, arg2));
        } else if (numberOfArguments == 3) {
            Object arg1 = pop();
            Object arg2 = pop();
            Object arg3 = pop();
            push(methodHandle.invoke(self, arg1, arg2, arg3));
        }
    }

    /**
     * Pops (removes) the given number of argument off the stack.
     *
     * @param numberOfArguments the argument to remove
     * @param isStatic          determines if a static method call is present. If not, an additional parameter for
     *                          "this" is popped off the stack.
     * @return the parameters which have been popped off the stack. Note that the first element will be "this" (unless
     * isStatic was <tt>true</tt>). Then the stack top will be the first argument and so on. Therefore, the compiler has
     * to actually reverse the execution order of arguments to match our expectation here.
     */
    private Object[] popArguments(int numberOfArguments, boolean isStatic) {
        int argsOffset = isStatic ? 0 : 1;
        if (argsOffset == 0 && numberOfArguments == 0) {
            return NO_ARGS;
        }

        Object[] args = new Object[numberOfArguments + argsOffset];
        if (!isStatic) {
            args[0] = pop();
        }
        for (int i = argsOffset; i < args.length; i++) {
            args[i] = pop();
        }
        return args;
    }

    private void invokeMacro(int numberOfArguments, Macro target) {
        Object[] args = new Object[numberOfArguments];
        for (int i = 0; i < args.length; i++) {
            args[i] = pop();
        }
        push(target.invoke(environment, args));
    }

    private void invokeConstructor(int numberOfArguments, Constructor<?> constructor) throws Exception {
        Object[] args = new Object[numberOfArguments];
        for (int i = 0; i < args.length; i++) {
            args[i] = pop();
        }
        push(constructor.newInstance(args));
    }

    @SuppressWarnings({"unchecked", "java:S1172"})
    @Explain("The type check is implicitly enforced and the JVM will raise a ClassCastException all by itself")
    private <T> T pop(Class<T> type) {
        return (T) pop();
    }

    private void push(Object object) {
        stack.add(object);
    }

    private Object pop() {
        if (stack.isEmpty()) {
            throw createVmError("Stack underflow");
        }
        return stack.remove(stack.size() - 1);
    }
}
