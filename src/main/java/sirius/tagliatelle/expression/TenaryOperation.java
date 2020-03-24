/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nullable;
import java.lang.reflect.Type;

/**
 * Represents a tenary operation liek {@code condition ? expr : expr}.
 * <p>
 * If the condition evaluates to <tt>true</tt> the first expression is evaluated and its result returned. Otherwise the
 * second expression is evaluated and its result is returned.
 */
public class TenaryOperation implements Expression {

    protected Expression conditionExpression;
    protected Expression leftExpression;
    protected Expression rightExpression;

    /**
     * Creates a new instance for the given condition and expressions.
     *
     * @param conditionExpression the expression to evaluate to determine which expression to evalaute and return
     * @param leftExpression      the expression to evaluate and return if the condition is <tt>true</tt>
     * @param rightExpression     the expression to evaluate and return if the condition is <tt>false</tt>
     */
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
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        this.conditionExpression = conditionExpression.propagateVisitor(visitor);
        this.leftExpression = leftExpression.propagateVisitor(visitor);
        this.rightExpression = rightExpression.propagateVisitor(visitor);

        return visitor.visitThis(this);
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

    @Nullable
    @Override
    public Type getGenericType() {
        return leftExpression.getGenericType();
    }
}
