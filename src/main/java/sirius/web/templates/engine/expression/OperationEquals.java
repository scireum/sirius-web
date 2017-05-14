/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.web.templates.engine.RenderContext;

import java.util.Objects;

/**
 * Created by aha on 12.05.17.
 */
public class OperationEquals extends Expression {

    protected Expression leftExpression;
    protected Expression rightExpression;
    protected boolean invert;

    public OperationEquals(Expression leftExpression, Expression rightExpression, boolean invert) {
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
        this.invert = invert;
    }

    @Override
    public Object eval(RenderContext ctx) {
        if (invert) {
            return Objects.equals(leftExpression.eval(ctx), rightExpression.eval(ctx));
        } else {
            return !Objects.equals(leftExpression.eval(ctx), rightExpression.eval(ctx));
        }
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public String toString() {
        if (invert) {
            return leftExpression + " != " + rightExpression;
        }
        return leftExpression + " == " + rightExpression;
    }
}
