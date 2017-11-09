/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.kernel.di.transformers.Transformable;
import sirius.tagliatelle.rendering.LocalRenderContext;

/**
 * Invokes {@link Transformable#as(Class)} on the given <tt>selfExpression</tt> and also provides the expected type at
 * compile time.
 * <p>
 * As currently generics aren't supported, <tt>.as()</tt> would yield <tt>Object.class</tt> as type which isn't
 * satisfying. As we use <tt>.as()</tt> as cast operator, we simply overload this behavior, if the
 * <tt>selfExpression</tt> is known to be a <tt>Transformable</tt>.
 */
public class TransformerCast implements Expression {

    private Expression selfExpression;
    private final Class<?> type;

    /**
     * Creates a new invokation and compile time cast for the given expression with the given type.
     *
     * @param selfExpression the expression to cast
     * @param type           the expected type
     */
    public TransformerCast(Expression selfExpression, Class<?> type) {
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
        return new TransformerCast(selfExpression.copy(), type);
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

            return ((Transformable) self).as(type);
        } catch (ClassCastException e) {
            throw new ExpressionEvaluationException(e);
        }
    }

    @Override
    public Class<?> getType() {
        return type;
    }
}
