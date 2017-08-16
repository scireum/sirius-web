/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.kernel.commons.Strings;
import sirius.tagliatelle.rendering.LocalRenderContext;

/**
 * Represents the <tt>noodle operation</tt>.
 * <p>
 * A noodle operation looks like {@code exprA | exprB}. If the first expression evaluates to <tt>null</tt> or to ""
 * the second expression is evaluated and its result returned. Otherwise the result of the first expression is returned.
 */
public class NoodleOperation extends Expression {

    protected Expression leftExpression;
    protected Expression rightExpression;

    /**
     * Creates a new operation for the given operands.
     *
     * @param leftExpression  the left operand
     * @param rightExpression the right operand
     */
    public NoodleOperation(Expression leftExpression, Expression rightExpression) {
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        Object leftValue = leftExpression.eval(ctx);
        if (Strings.isFilled(leftValue)) {
            return leftValue;
        }

        return rightExpression.eval(ctx);
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

        if (leftExpression.isConstant()) {
            Object leftValue = leftExpression.eval(null);
            if (Strings.isEmpty(leftValue)) {
                return rightExpression;
            } else {
                return leftExpression;
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
        return new NoodleOperation(leftExpression.copy(), rightExpression.copy());
    }

    @Override
    public Class<?> getType() {
        return leftExpression.getType();
    }
}
