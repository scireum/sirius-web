/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.LocalRenderContext;

/**
 * Created by aha on 10.05.17.
 */
public abstract class BooleanOperation extends Expression {

    protected Expression leftExpression;
    protected Expression rightExpression;

    public BooleanOperation(Expression leftExpression, Expression rightExpression) {
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
        //TODO ensure boolean
    }

    @Override
    public Expression visit(ExpressionVisitor visitor) {
        this.leftExpression = visitor.visit(leftExpression);
        this.rightExpression = visitor.visit(rightExpression);

        return visitor.visit(this);
    }

    @Override
    public Expression reduce() {
        this.leftExpression = leftExpression.reduce();
        this.rightExpression = rightExpression.reduce();

        return this;
    }


    protected boolean eval(Expression expr, LocalRenderContext ctx) {
        return (boolean)expr.eval(ctx);
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }
}
