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
public class BinaryOperation extends Expression {

    private Expression leftExpression;
    private Expression rightExpression;

    @Override
    public Object eval(RenderContext ctx) {
        Object left = leftExpression.eval(ctx);
        Object right = rightExpression.eval(ctx);


        return null;
    }

    @Override
    public Class<?> getType() {
        return Boolean.class;
    }
}
