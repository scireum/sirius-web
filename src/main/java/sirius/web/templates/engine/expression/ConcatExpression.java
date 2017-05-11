/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.web.templates.engine.RenderContext;

import java.util.List;

/**
 * Created by aha on 10.05.17.
 */
public class ConcatExpression extends Expression {

    private List<Expression> stringExpressions;

    @Override
    public Object eval(RenderContext ctx) {
        StringBuilder sb = new StringBuilder();
        for (Expression expr : stringExpressions) {
            Object result = expr.eval(ctx);
            if (result != null) {
                sb.append(result);
            }
        }
        return sb.toString();
    }

    @Override
    public Class<?> getType() {
        return String.class;
    }
}
