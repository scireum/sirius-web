/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.web.templates.engine.RenderContext;

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

    protected boolean eval(Expression expr, RenderContext ctx) {
        return (boolean)expr.eval(ctx);
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }
}
