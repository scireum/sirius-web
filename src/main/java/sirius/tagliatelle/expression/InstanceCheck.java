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
 * Represents an <tt>instanceof</tt> check.
 */
public class InstanceCheck implements Expression {

    private Expression selfExpression;
    private final Class<?> type;

    /**
     * Creates a new <tt>instanceof</tt> for the given expression with the given type.
     *
     * @param selfExpression the expression to check
     * @param type           the expected type
     */
    public InstanceCheck(Expression selfExpression, Class<?> type) {
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
        if (selfExpression.isConstant()) {
            Object value = selfExpression.eval(null);
            if (value != null && type.isAssignableFrom(value.getClass())) {
                return ConstantBoolean.TRUE;
            } else {
                return ConstantBoolean.FALSE;
            }
        }

        return this;
    }

    @Override
    public Expression copy() {
        return new InstanceCheck(selfExpression.copy(), type);
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        Object self = selfExpression.eval(ctx);
        return type.isAssignableFrom(self.getClass());
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }
}
