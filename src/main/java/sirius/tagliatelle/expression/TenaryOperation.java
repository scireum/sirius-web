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
 * Created by aha on 24.05.17.
 */
public class TenaryOperation extends Expression {

    protected Expression conditionExpression;
    protected Expression leftExpression;
    protected Expression rightExpression;

    public TenaryOperation(Expression conditionExpression, Expression leftExpression, Expression rightExpression) {
        this.conditionExpression = conditionExpression;
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        boolean condition = (boolean) conditionExpression.eval(ctx);
        if (condition) {
            return leftExpression.eval(ctx);
        } else {
            return rightExpression.eval(ctx);
        }
    }

    @Override
    public Expression visit(ExpressionVisitor visitor) {
        this.conditionExpression = visitor.visit(conditionExpression);
        this.leftExpression = visitor.visit(leftExpression);
        this.rightExpression = visitor.visit(rightExpression);

        return visitor.visit(this);
    }

    @Override
    public Expression reduce() {
        this.conditionExpression = conditionExpression.reduce();
        this.leftExpression = leftExpression.reduce();
        this.rightExpression = rightExpression.reduce();

        if (conditionExpression.isConstant()) {
            boolean condition = (boolean) conditionExpression.eval(null);
            if (condition) {
                return leftExpression;
            } else {
                return rightExpression;
            }
        }

        return this;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Expression copy() {
        return new TenaryOperation(conditionExpression.copy(), leftExpression.copy(), rightExpression.copy());
    }

    @Override
    public Class<?> getType() {
        return leftExpression.getType();
    }
}
