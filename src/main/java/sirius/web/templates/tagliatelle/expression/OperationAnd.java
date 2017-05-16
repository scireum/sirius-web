/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.expression;

import sirius.web.templates.tagliatelle.LocalRenderContext;

/**
 * Created by aha on 12.05.17.
 */
public class OperationAnd extends BooleanOperation {

    public OperationAnd(Expression leftExpression, Expression rightExpression) {
        super(leftExpression, rightExpression);
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return eval(leftExpression, ctx) && eval(rightExpression, ctx);
    }

    @Override
    public String toString() {
        return leftExpression + " && " + rightExpression;
    }
}
