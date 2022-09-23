/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;

/**
 * Represents an OR operation.
 * <p>
 * This generates custome code, as it first evaluates the left operand and then only evaluates the right
 * operand if the left one was <tt>false</tt>.
 */
public class Disjunction extends BinaryOperation {

    /**
     * Creates a new disjunction.
     *
     * @param position the position in the source code
     * @param left     the left operand
     * @param right    the right operand
     */
    public Disjunction(Position position, Node left, Node right) {
        super(position, null, left, right, boolean.class);
    }

    @Override
    public void emit(Assembler assembler) {
        left.emit(assembler);
        Assembler.Label falseLabel = assembler.createLabel();
        Assembler.Label endLabel = assembler.createLabel();

        assembler.emitJump(OpCode.JMP_FALSE, falseLabel, getPosition());
        assembler.emitPushConstant(true, position);
        assembler.emitJump(OpCode.JMP, endLabel, getPosition());
        falseLabel.define();
        right.emit(assembler);
        endLabel.define();
    }
}
