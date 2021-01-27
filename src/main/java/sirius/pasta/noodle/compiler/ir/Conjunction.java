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

/**
 * Represents an AND operation.
 * <p>
 * This generates custom code, as it first evaluates the left operand and then only evaluates the right
 * operand if the left one was <tt>true</tt>.
 */
public class Conjunction extends BinaryOperation {

    /**
     * Creates a new conjunction.
     *
     * @param position the position in the source code
     * @param left     the left operand
     * @param right    the right operand
     */
    public Conjunction(Position position, Node left, Node right) {
        super(position, null, left, right, boolean.class);
    }

    @Override
    public void emit(Assembler assembler) {
        left.emit(assembler);
        Assembler.Label falseLabel = assembler.createLabel();
        assembler.emitJump(OpCode.JMP_FALSE, falseLabel, getPosition());
        right.emit(assembler);
        Assembler.Label endLabel = assembler.createLabel();
        assembler.emitJump(OpCode.JMP, endLabel, getPosition());
        falseLabel.define();
        assembler.emitPushConstant(false, position);
        endLabel.define();
    }
}
