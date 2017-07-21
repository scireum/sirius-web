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
 * Represents a binary operation on boolean types.
 */
public abstract class BooleanOperation extends Expression {

    protected Expression leftExpression;
    protected Expression rightExpression;

    /**
     * Creates a new operation for the given operands.
     *
     * @param leftExpression  the left operand which must be a boolean expression
     * @param rightExpression the right operand which must be a boolean expression
     */
    public BooleanOperation(Expression leftExpression, Expression rightExpression) {
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
    }

    @Override
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        this.leftExpression = leftExpression.propagateVisitor(visitor);
        this.rightExpression = rightExpression.propagateVisitor(visitor);

        return visitor.visitThis(this);
    }

    @Override
    public Expression reduce() {
        this.leftExpression = leftExpression.reduce();
        this.rightExpression = rightExpression.reduce();

        return this;
    }

    /**
     * Evaluates the operation into a boolean.
     *
     * @param expr the expression to evaluate
     * @param ctx  the context used for the evaluation
     * @return the result of the expression casted as boolean
     */
    protected boolean eval(Expression expr, LocalRenderContext ctx) {
        return (boolean) expr.eval(ctx);
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }
}
