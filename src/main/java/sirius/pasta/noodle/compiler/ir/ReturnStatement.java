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
import sirius.pasta.noodle.compiler.CompilationContext;

import java.lang.reflect.Type;

/**
 * Returns the value of an expression.
 */
public class ReturnStatement extends Statement {

    private Node expression;

    /**
     * Creates a new return statement.
     *
     * @param position the position in the source code
     */
    public ReturnStatement(Position position) {
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

    @Override
    public Type getGenericType() {
        return getExpression().getGenericType();
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
