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

import javax.annotation.Nullable;

/**
 * Returns the value of an expression.
 */
public class ReturnNode extends Node {

    private Node expression;

    /**
     * Creates a new return node.
     *
     * @param position the position in the source code
     */
    public ReturnNode(Position position) {
        super(position);
    }

    /**
     * Returns the expression being returned.
     *
     * @return the expression being returned
     */
    public Node getExpression() {
        return expression;
    }

    /**
     * Specifies the expression being returned.
     *
     * @param expression the expression being returned
     */
    public void setExpression(Node expression) {
        this.expression = expression;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        this.expression = this.expression.reduce(compilationContext);
        return this;
    }

    @Nullable
    @Override
    public Class<?> getType() {
        return getExpression().getType();
    }

    @Override
    public void emit(Assembler assembler) {
        getExpression().emit(assembler);
        assembler.emitByteCode(OpCode.RET_STACK_TOP, 0, getPosition());
    }

    @Override
    public String toString() {
        return "ReturnNode: " + expression;
    }
}
