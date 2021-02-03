/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.pasta.noodle.MethodPointer;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.VariableScoper;

import javax.annotation.Nullable;
import java.lang.reflect.Type;
import java.util.Iterator;

/**
 * Represents an IF statement.
 */
public class ForStatement extends Statement {

    private Node condition;
    private VariableScoper.Variable loopVariable;
    private VariableScoper.Variable iteratorVariable;
    private Node loopBlock;

    /**
     * Creates a new for statement.
     *
     * @param position the position in the source code
     */
    public ForStatement(Position position) {
        super(position);
    }

    public Node getCondition() {
        return condition;
    }

    public void setCondition(Node condition) {
        this.condition = condition;
    }

    public VariableScoper.Variable getLoopVariable() {
        return loopVariable;
    }

    public void setLoopVariable(VariableScoper.Variable loopVariable) {
        this.loopVariable = loopVariable;
    }

    public VariableScoper.Variable getIteratorVariable() {
        return iteratorVariable;
    }

    public void setIteratorVariable(VariableScoper.Variable iteratorVariable) {
        this.iteratorVariable = iteratorVariable;
    }

    public Node getLoopBlock() {
        return loopBlock;
    }

    public void setLoopBlock(Node loopBlock) {
        this.loopBlock = loopBlock;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        this.condition = this.condition.reduce(compilationContext);
        this.loopBlock = this.loopBlock.reduce(compilationContext);

        return this;
    }

    @Override
    public void emit(Assembler assembler) {
        try {
            Assembler.Label startLabel = assembler.createLabel();
            Assembler.Label endLabel = assembler.createLabel();

            condition.emit(assembler);
            assembler.emitPushConstant(new MethodPointer(Iterable.class.getMethod("iterator")), position);
            assembler.emitByteCode(OpCode.INVOKE, 0, position);
            assembler.emitByteCode(OpCode.POP_VARIABLE, iteratorVariable.getLocalIndex(), getPosition());
            startLabel.define();
            assembler.emitByteCode(OpCode.PUSH_VARIABLE, iteratorVariable.getLocalIndex(), getPosition());
            assembler.emitPushConstant(new MethodPointer(Iterator.class.getMethod("hasNext")), position);
            assembler.emitByteCode(OpCode.INVOKE, 0, position);
            assembler.emitJump(OpCode.JMP_FALSE, endLabel, position);
            assembler.emitByteCode(OpCode.PUSH_VARIABLE, iteratorVariable.getLocalIndex(), getPosition());
            assembler.emitPushConstant(new MethodPointer(Iterator.class.getMethod("next")), position);
            assembler.emitByteCode(OpCode.INVOKE, 0, position);
            assembler.emitByteCode(OpCode.POP_VARIABLE, loopVariable.getLocalIndex(), getPosition());
            loopBlock.emit(assembler);
            assembler.emitJump(OpCode.JMP_BACK, startLabel, position);
            endLabel.define();
        } catch (IllegalAccessException | NoSuchMethodException e) {
            // This almost certainly cannot happen, therefore we handle it quite rude
            throw new IllegalStateException(Strings.apply(
                    "Failed to use reflection in order to setup loop construct for: %s",
                    this));
        }
    }

    @Override
    public String toString() {
        return new StringBuilder("for (").append(loopVariable.getType())
                                         .append(" ")
                                         .append(loopVariable.getName())
                                         .append(" : ")
                                         .append(condition)
                                         .append(") { ")
                                         .append(loopBlock)
                                         .append(" }")
                                         .toString();
    }
}
