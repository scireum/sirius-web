/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;

import java.lang.reflect.Type;

/**
 * Represents a binary operation.
 * <p>
 * The operation itself is defined by the actual op code to be executed.
 */
public class BinaryOperation extends Node {

    protected OpCode opCode;
    protected Node left;
    protected Node right;
    protected Class<?> returnType;

    /**
     * Creates a new numeric binary operation.
     *
     * @param position the position within the source code
     * @param opCode   the operation to perform
     * @param left     the left operand
     * @param right    the right operand
     */
    public BinaryOperation(Position position, OpCode opCode, Node left, Node right) {
        this(position, opCode, left, right, CompilationContext.coerceNumericTypes(left.getType(), right.getType()));
    }

    /**
     * Creates a new binary operation with a custom return type.
     *
     * @param position   the position within the source code
     * @param opCode     the operation to perform
     * @param left       the left operand
     * @param right      the right operand
     * @param returnType the actual result type of this operation
     */
    public BinaryOperation(Position position, OpCode opCode, Node left, Node right, Class<?> returnType) {
        super(position);
        this.opCode = opCode;
        this.left = left;
        this.right = right;
        this.returnType = returnType;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        left = left.reduce(compilationContext);
        right = right.reduce(compilationContext);

        if (left.isConstant() && right.isConstant()) {
            return foldConstants();
        }

        return this;
    }

    private Node foldConstants() {
        Object a = left.getConstantValue();
        Object b = right.getConstantValue();
        if (opCode == OpCode.OP_ADD) {
            return reduceConstantAdd(a, b);
        }
        if (opCode == OpCode.OP_SUB) {
            return reduceConstantSub(a, b);
        }
        if (opCode == OpCode.OP_CONCAT) {
            return reduceConstantConcat(a, b);
        }

        return this;
    }

    private Constant reduceConstantConcat(Object a, Object b) {
        if (a == null && b == null) {
            return new Constant(left.position, "");
        }
        if (a == null) {
            return new Constant(left.position, b.toString());
        }
        if (b == null) {
            return new Constant(left.position, a.toString());
        }
        return new Constant(left.position, a.toString() + b);
    }

    private Node reduceConstantSub(Object a, Object b) {
        if (a instanceof Integer) {
            return new Constant(left.position, ((int) a) - (int) b);
        }
        if (a instanceof Long) {
            return new Constant(left.position, ((long) a) - (long) b);
        }
        if (a instanceof Double) {
            return new Constant(left.position, ((double) a) - (double) b);
        }
        return this;
    }

    private Node reduceConstantAdd(Object a, Object b) {
        if (a instanceof Integer) {
            return new Constant(left.position, ((int) a) + (int) b);
        }
        if (a instanceof Long) {
            return new Constant(left.position, ((long) a) + (long) b);
        }
        if (a instanceof Double) {
            return new Constant(left.position, ((double) a) + (double) b);
        }
        return this;
    }

    @Override
    public Type getGenericType() {
        return returnType;
    }

    @Override
    public void emit(Assembler assembler) {
        right.emit(assembler);
        assembler.coerce(position, right.getType(), left.getType());
        left.emit(assembler);
        assembler.coerce(position, left.getType(), right.getType());
        assembler.emitByteCode(opCode, 0, position);
    }

    @Override
    public String toString() {
        return "BinaryOperation: " + left + " " + opCode.name() + " " + right;
    }
}
