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
 * Represents an IF statement.
 */
public class IfStatement extends Statement {

    private Node condition;
    private Node trueBlock;
    private Node falseBlock;

    /**
     * Creates a new if statement.
     *
     * @param position the position in the source code
     */
    public IfStatement(Position position) {
        super(position);
    }

    public Node getCondition() {
        return condition;
    }

    public void setCondition(Node condition) {
        this.condition = condition;
    }

    public Node getTrueBlock() {
        return trueBlock;
    }

    public void setTrueBlock(Node trueBlock) {
        this.trueBlock = trueBlock;
    }

    public Node getFalseBlock() {
        return falseBlock;
    }

    public void setFalseBlock(Node falseBlock) {
        this.falseBlock = falseBlock;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        this.condition = this.condition.reduce(compilationContext);
        this.trueBlock = this.trueBlock.reduce(compilationContext);
        if (this.falseBlock != null) {
            this.falseBlock = this.falseBlock.reduce(compilationContext);
        }

        return this;
    }

    @Override
    public void emit(Assembler assembler) {
        Assembler.Label falseLabel = assembler.createLabel();
        Assembler.Label endLabel = assembler.createLabel();

        condition.emit(assembler);
        assembler.emitJump(OpCode.JMP_FALSE, falseLabel, getPosition());
        trueBlock.emit(assembler);
        if (falseBlock != null) {
            assembler.emitJump(OpCode.JMP, endLabel, falseBlock.getPosition());
        }
        falseLabel.define();
        if (falseBlock != null) {
            falseBlock.emit(assembler);
        }
        endLabel.define();
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder("if (").append(condition).append(") { ").append(trueBlock);
        if (falseBlock != null) {
            result.append(" } else { ");
            result.append(falseBlock);
        }
        result.append("}");
        return result.toString();
    }
}
