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
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.macros.Macro;
import sirius.web.security.UserContext;

import java.lang.invoke.MethodHandle;
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
        this.compiledMethod = method;
        this.environment = environment;
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
                case POP_VARIABLE:
                    environment.writeVariable(index, pop());
                    return -1;
                case JMP_FALSE:
                    if (Boolean.FALSE.equals(pop())) {
                        instructionPointer += index;
                    }
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
                    push(pop(Class.class).cast(pop()));
                    break;
                case OP_INSTANCE_OF:
                    push(pop(Class.class).isAssignableFrom(pop().getClass()));
                    break;
                case OP_COERCE_INT_TO_LONG:
                    push(Long.valueOf(pop(int.class)));
                    break;
                case OP_COERCE_INT_TO_DOUBLE:
                    push(Double.valueOf(pop(int.class)));
                    break;
                case OP_COERCE_LONG_TO_DOUBLE:
                    push(Double.valueOf(pop(long.class)));
                    break;
                case OP_INTRINSIC_TRANSFORMABLE_AS:
                    handleTransformableAs();
                    break;
                case OP_INTRINSIC_TRANSFORMABLE_IS:
                    handleTransformableIs();
                    break;
                case OP_INTRINSIC_STRINGS_IS_EMPTY:
                    push(Strings.isEmpty(pop()));
                    break;
                case OP_INTRINSIC_STRINGS_IS_FILLED:
                    push(Strings.isFilled(pop()));
                    break;
                case OP_INTRINSIC_VALUE_OF:
                    push(Value.of(pop()));
                    break;
                case OP_INTRINSIC_NLS_GET:
                    push(NLS.get(pop(String.class)));
                    break;
                case OP_INTRINSIC_USER_CONTEXT_HELPER:
                    push(UserContext.getHelper(pop(Class.class)));
                    break;
                case OP_INTRINSIC_USER_CONTEXT_CURRENT_USER:
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
        for (int i = 1; i < position.getPos(); i++) {
            result.append(" ");
        }
        result.append("^\n");
        return result.toString();
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

        throw new IllegalArgumentException(Strings.apply("Cannot add %s and %s (%s, %s)",
                                                         a,
                                                         b,
                                                         a.getClass(),
                                                         b.getClass()));
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

        throw new IllegalArgumentException(Strings.apply("Cannot subtract %s minus %s (%s, %s)",
                                                         a,
                                                         b,
                                                         a.getClass(),
                                                         b.getClass()));
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

        throw new IllegalArgumentException(Strings.apply("Cannot multiply %s by %s (%s, %s)",
                                                         a,
                                                         b,
                                                         a.getClass(),
                                                         b.getClass()));
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

        throw new IllegalArgumentException(Strings.apply("Cannot divide %s by %s (%s, %s)",
                                                         a,
                                                         b,
                                                         a.getClass(),
                                                         b.getClass()));
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

        throw new IllegalArgumentException(Strings.apply("Cannot compute the modulo of %s and %s (%s, %s)",
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
        if (target instanceof MethodHandle) {
            invokeMethod(numberOfArguments, isStatic, (MethodHandle) target);
        } else if (target instanceof Macro) {
            invokeMacro(numberOfArguments, (Macro) target);
        }
    }

    private void invokeMethod(int numberOfArguments, boolean isStatic, MethodHandle methodHandle) throws Throwable {
        if (methodHandle.isVarargsCollector() || numberOfArguments > 3) {
            invokeMethodWithArguments(numberOfArguments, isStatic, methodHandle);
            return;
        }

        if (isStatic) {
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
        } else {
            Object self = pop();
            if (self == null) {
                while(numberOfArguments-- > 0) {
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
    }

    private void invokeMethodWithArguments(int numberOfArguments, boolean isStatic, MethodHandle methodHandle)
            throws Throwable {
        Object[] args = popArguments(numberOfArguments, isStatic);
        push(methodHandle.invokeWithArguments(args));
    }

    /**
     * Pops (removes) the given number of argument off the stack.
     *
     * @param numberOfArguments the argument to remove
     * @param isStatic          determines if a static method call is present. If not, an additional parameter for "this" is
     *                          popped off the stack.
     * @return the parameters which have been popped off the stack. Note that the first element will be "this" (unless
     * isStatic was <tt>true</tt>). Then the stack top will be the first argument and so on. Therefore the compiler has
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

    @SuppressWarnings({"unchecked", "java:S1172"})
    @Explain("The type check is implicitely enforced and the JVM will raise a ClassCastException all by itself")
    private <T> T pop(Class<T> type) {
        return (T) pop();
    }

    private void push(Object object) {
        stack.add(object);
    }

    private Object pop() {
        if (stack.isEmpty()) {
            throw new IllegalStateException("Stack underflow");
        }
        return stack.remove(stack.size() - 1);
    }
}
