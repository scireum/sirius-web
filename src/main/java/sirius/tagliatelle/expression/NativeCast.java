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
 * Represents a native (Java) cast operation to provide a type information at compile time.
 * <p>
 * The evaluation itself, only evaluates the <tt>selfExpression</tt> and verifies that the result has the expected type.
 */
public class NativeCast extends Expression {

    private Expression selfExpression;
    private final Class<?> type;

    /**
     * Creates a new cast for the given expression with the given type.
     *
     * @param selfExpression the expression to cast
     * @param type           the expected type
     */
    public NativeCast(Expression selfExpression, Class<?> type) {
        this.selfExpression = selfExpression;
        this.type = type;
    }

    @Override
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        this.selfExpression = selfExpression.propagateVisitor(visitor);
        return visitor.visitThis(this);
    }

    @Override
    public Expression reduce() {
        this.selfExpression = selfExpression.reduce();
        return this;
    }

    @Override
    public Expression copy() {
        return new NativeCast(selfExpression.copy(), type);
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        try {
            Object self = selfExpression.eval(ctx);
            if (self == null) {
                return null;
            }

            if (!type.isAssignableFrom(self.getClass())) {
                throw new ClassCastException(self.getClass().getName());
            }
            return self;
        } catch (ClassCastException e) {
            throw new ExpressionEvaluationException(e);
        }
    }

    @Override
    public Class<?> getType() {
        return type;
    }
}
