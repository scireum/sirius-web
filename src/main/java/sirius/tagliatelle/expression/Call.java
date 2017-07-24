/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import java.util.List;

/**
 * Represents an invocation with parameters.
 */
public abstract class Call extends Expression {

    /**
     * A placeholder to represent "no arguments", which is preferred over <tt>null</tt> or creating an empty array each
     * time.
     */
    public static final Expression[] NO_ARGS = {};
    protected Expression[] parameterExpressions = NO_ARGS;

    @Override
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        for (int i = 0; i < parameterExpressions.length; i++) {
            parameterExpressions[i] = parameterExpressions[i].propagateVisitor(visitor);
        }

        return visitor.visitThis(this);
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    protected void copyParametersTo(Call target) {
        if (parameterExpressions == NO_ARGS) {
            return;
        }

        target.parameterExpressions = new Expression[parameterExpressions.length];
        for (int i = 0; i < parameterExpressions.length; i++) {
            target.parameterExpressions[i] = parameterExpressions[i].copy();
        }
    }

    /**
     * Applies the parameters to evaluate and pass the to invocation.
     *
     * @param parameters the parameters to apply
     */
    public void setParameters(List<Expression> parameters) {
        if (parameters != null && !parameters.isEmpty()) {
            this.parameterExpressions = parameters.toArray(new Expression[parameters.size()]);
        }
    }
}
