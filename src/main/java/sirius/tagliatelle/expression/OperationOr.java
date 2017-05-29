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
 * Created by aha on 12.05.17.
 */
public class OperationOr extends BooleanOperation {

    public OperationOr(Expression leftExpression, Expression rightExpression) {
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
        return new OperationOr(leftExpression.copy(), rightExpression.copy());
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
