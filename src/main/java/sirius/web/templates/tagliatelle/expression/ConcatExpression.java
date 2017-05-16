/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.expression;

import sirius.web.templates.tagliatelle.LocalRenderContext;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created by aha on 10.05.17.
 */
public class ConcatExpression extends Expression {

    private List<Expression> stringExpressions = new ArrayList<>();

    public ConcatExpression(Expression... expressions) {
        stringExpressions.addAll(Arrays.asList(expressions));
    }

    public void add(Expression expr) {
        stringExpressions.add(expr);
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
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

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Expression expr : stringExpressions) {
            if (sb.length() > 0) {
                sb.append(" + ");
            }
            sb.append(expr);
        }

        return sb.toString();
    }
}
