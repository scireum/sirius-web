/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;

import javax.annotation.Nullable;
import java.lang.reflect.Type;

/**
 * Represents a tenary operation.
 */
public class TernaryOperation extends Node {

    private final Node condition;
    private final Node whenTrue;
    private final Node whenFalse;

    /**
     * Creates a new tenary operation.
     *
     * @param condition the condition to check
     * @param whenTrue  the expression to evaluate when the condition is <tt>true</tt>
     * @param whenFalse the expression to evaluate when the condition is <tt>false</tt>
     */
    public TernaryOperation(Node condition, Node whenTrue, Node whenFalse) {
        super(condition.getPosition());

        this.condition = condition;
        this.whenTrue = whenTrue;
        this.whenFalse = whenFalse;
    }

    @Nullable
    @Override
    public Class<?> getType() {
        if (!CompilationContext.isAssignableTo(whenTrue.getType(), whenFalse.getType())) {
            return whenFalse.getType();
        } else {
            return whenTrue.getType();
        }
    }

    @Nullable
    @Override
    public Type getGenericType() {
        if (!CompilationContext.isAssignableTo(whenTrue.getType(), whenFalse.getType())) {
            return whenFalse.getGenericType();
        } else {
            return whenTrue.getGenericType();
        }
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        if (condition.isConstant()) {
            if (Boolean.TRUE.equals(condition.getConstantValue())) {
                return whenTrue;
            } else {
                return whenFalse;
            }
        }

        return this;
    }

    @Override
    public void emit(Assembler assembler) {
        condition.emit(assembler);
        Assembler.Label falseLabel = assembler.createLabel();
        assembler.emitJump(OpCode.JMP_FALSE, falseLabel, getPosition());
        whenTrue.emit(assembler);
        Assembler.Label endLabel = assembler.createLabel();
        assembler.emitJump(OpCode.JMP, endLabel, whenFalse.getPosition());
        falseLabel.define();
        whenFalse.emit(assembler);
        endLabel.define();
    }

    @Override
    public String toString() {
        return "TenaryOperation:" + condition + " ? " + whenTrue + " : " + whenFalse;
    }
}
