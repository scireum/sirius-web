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

/**
 * Represents an unary operation.
 */
public class UnaryOperation extends Node {

    private OpCode opCode;
    private Node operand;

    /**
     * Creates a new unary operation for the given op code.
     *
     * @param position the position in the source code
     * @param opCode   the operation to perform
     * @param operand  the operand to perform the operation on
     */
    public UnaryOperation(Position position, OpCode opCode, Node operand) {
        super(position);
        this.opCode = opCode;
        this.operand = operand;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        this.operand = operand.reduce(compilationContext);
        return this;
    }

    @Override
    public Class<?> getType() {
        return operand.getType();
    }

    @Override
    public void emit(Assembler assembler) {
        operand.emit(assembler);
        assembler.emitByteCode(opCode, 0, position);
    }

    @Override
    public String toString() {
        return "UnaryOperation: " + opCode + "(" + operand + ")";
    }
}
