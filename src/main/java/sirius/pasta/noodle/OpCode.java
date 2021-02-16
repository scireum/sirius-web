/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

/**
 * Defines all op codes understood by the {@link Invocation interpreter}.
 * <p>
 * A bytecode used by the interpreter is a 64bit integer. The lower two bytes make up for an index (see below)
 * the 3rd byte is the actual index of the list of op codes below.
 */
public enum OpCode {

    /**
     * Pushes the constant with the given index on the stack.
     */
    PUSH_CONST,

    /**
     * Pushes the shared variable with the given index on the stack.
     */
    PUSH_VARIABLE,

    /**
     * Pops a value off the stack and stores it in the shared variable with the given index.
     */
    POP_VARIABLE,

    /**
     * Pushes a field on the stack.
     */
    PUSH_FIELD,

    /**
     * Pops a stack value into a field.
     */
    POP_FIELD,

    /**
     * Pops a value off the stack and throws it away.
     */
    POP_TOP,

    /**
     * Pops off a value of the stack. If it is false, the instruction pointer is incremented by the given index.
     */
    JMP_FALSE,

    /**
     * Increments the instruction pointer by the given index.
     */
    JMP,

    /**
     * Decrements the instruction pointer by the given index.
     */
    JMP_BACK,

    /**
     * Invokes a <tt>Method</tt>.
     * <p>
     * This first pops the invocation target (MethodHandle), followed by "this" and then  the number of arguments
     * (as determined by the index) off the stack. It then invokes the target and pushes the result on the stack.
     */
    INVOKE,

    /**
     * Invokes either a {@link sirius.pasta.noodle.macros.Macro} or a <tt>Method</tt>.
     * <p>
     * This first pops the invocation target (macro or MethodHandle), followed by the number of arguments
     * (as determined by the index) off the stack. It then invokes the target and pushes the result on the stack.
     */
    INVOCE_STATIC,

    /**
     * Pushes a built-in constant on the stack.
     */
    PUSH_BUILT_IN,

    /**
     * Creates a lambda closure.
     */
    LAMBDA,

    /**
     * Returns the stack top.
     */
    RET_STACK_TOP,

    /**
     * Pops two values off the stack and pushes the result of Objects.equals(a, b).
     */
    OP_EQ,

    /**
     * Pops two values off the stack and pushes the result of a == b.
     */
    OP_ID,

    /**
     * Pops two values off the stack and pushes the result of !Objects.equals(a, b).
     */
    OP_NE,

    /**
     * Pops two values off the stack and pushes the result of a &gt; b.
     */
    OP_GT,

    /**
     * Pops two values off the stack and pushes the result of a &gt;= b.
     */
    OP_GE,

    /**
     * Pops two values off the stack and pushes the result of a &lt; b.
     */
    OP_LT,

    /**
     * Pops two values off the stack and pushes the result of a &lt;= b.
     */
    OP_LE,

    /**
     * Pops a boolean value off the stack and pushes !a on the stack.
     */
    OP_NOT,

    /**
     * Pops two values off the stack and pushes the result of a + b.
     */
    OP_ADD,

    /**
     * Pops two values off the stack and pushes the result of a - b.
     */
    OP_SUB,

    /**
     * Pops two values off the stack and pushes the result of a * b.
     */
    OP_MUL,

    /**
     * Pops two values off the stack and pushes the result of a / b.
     */
    OP_DIV,

    /**
     * Pops two values off the stack and pushes the result of a % b.
     */
    OP_MOD,

    /**
     * Pops two values off the stack and pushes the result of String.valueOf(a) + String.valueOf(b).
     */
    OP_CONCAT,

    /**
     * Pops a class and a value off the stack and pushes the value casted to the class.
     */
    OP_CAST,

    /**
     * Pops a class and a value off the stack and pushes the result of a instanceof b on the stack.
     */
    OP_INSTANCE_OF,

    /**
     * Pops an int off the stack and pushes it as long back on the stack.
     */
    COERCE_INT_TO_LONG,

    /**
     * Pops an int off the stack and pushes it as double back on the stack.
     */
    COERCE_INT_TO_DOUBLE,

    /**
     * Pops a long off the stack and pushes it as double back on the stack.
     */
    COERCE_LONG_TO_DOUBLE,

    /**
     * Pops a class and a value off the stack and pushes the result of ((Transformable)b).as(a) on the stack.
     */
    INTRINSIC_TRANSFORMABLE_AS,

    /**
     * Pops a class and a value off the stack and pushes the result of ((Transformable)b).is(a) on the stack.
     */
    INTRINSIC_TRANSFORMABLE_IS,

    /**
     * Pops a value off the stack and pushes the result of Strings.isEmpty(a) on the stack.
     */
    INTRINSIC_STRINGS_IS_EMPTY,

    /**
     * Pops a value off the stack and pushes the result of Strings.isFilled(a) on the stack.
     */
    INTRINSIC_STRINGS_IS_FILLED,

    /**
     * Pops a value off the stack and pushes the result of Value.of(a) on the stack.
     */
    INTRINSIC_VALUE_OF,

    /**
     * Pops a value off the stack and pushes the result of NLS.get(a) on the stack.
     */
    INTRINSIC_NLS_GET,

    /**
     * Pops a value off the stack and pushes the result of UserContext.getHelper(a) on the stack.
     */
    INTRINSIC_USER_CONTEXT_HELPER,

    /**
     * Pushes the result of UserContext.getCurrentUser() on the stack.
     */
    INTRINSIC_USER_CONTEXT_CURRENT_USER
}
