/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.rendering.LocalRenderContext;

/**
 * Represents a boolean <tt>and</tt>.
 */
public class OperationAnd extends BooleanOperation {

    /**
     * Creates a new <tt>and</tt> operation for the given operands.
     *
     * @param leftExpression  the left operand
     * @param rightExpression the right operand
     */
    public OperationAnd(Expression leftExpression, Expression rightExpression) {
        super(leftExpression, rightExpression);
    }

    @Override
    public Expression reduce() {
        super.reduce();

        if (ConstantBoolean.TRUE.equals(leftExpression)) {
            return rightExpression;
        }
        if (ConstantBoolean.FALSE.equals(leftExpression)) {
            return ConstantBoolean.FALSE;
        }

        return this;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Expression copy() {
        return new OperationAnd(leftExpression.copy(), rightExpression.copy());
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return eval(leftExpression, ctx) && eval(rightExpression, ctx);
    }

    @Override
    public String toString() {
        return leftExpression + " && " + rightExpression;
    }
}
