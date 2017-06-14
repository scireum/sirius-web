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
 * Represents a boolean <tt>or</tt>.
 */
public class OrOperation extends BooleanOperation {

    /**
     * Creates a new <tt>or</tt> operation for the given operands.
     *
     * @param leftExpression  the left operand
     * @param rightExpression the right operand
     */
    public OrOperation(Expression leftExpression, Expression rightExpression) {
        super(leftExpression, rightExpression);
    }

    @Override
    public Expression reduce() {
        super.reduce();

        if (ConstantBoolean.TRUE.equals(leftExpression)) {
            return ConstantBoolean.TRUE;
        }

        if (ConstantBoolean.TRUE.equals(rightExpression)) {
            return ConstantBoolean.TRUE;
        }

        if (ConstantBoolean.FALSE.equals(leftExpression) && ConstantBoolean.FALSE.equals(rightExpression)) {
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
        return new OrOperation(leftExpression.copy(), rightExpression.copy());
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return eval(leftExpression, ctx) || eval(rightExpression, ctx);
    }

    @Override
    public String toString() {
        return leftExpression + " || " + rightExpression;
    }
}
