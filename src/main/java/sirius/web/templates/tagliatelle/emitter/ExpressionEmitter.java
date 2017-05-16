/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.emitter;

import sirius.web.templates.tagliatelle.LocalRenderContext;
import sirius.web.templates.tagliatelle.expression.Expression;

/**
 * Created by aha on 10.05.17.
 */
public class ExpressionEmitter implements Emitter {

    private Expression expression;

    public ExpressionEmitter(Expression expression) {
        this.expression = expression;
    }

    @Override
    public void emit(LocalRenderContext context) {
        Object value = expression.eval(context);
        if (value == null) {
            context.output("");
        } else {
            context.output(value.toString());
        }
    }

    @Override
    public String toString() {
        return "@(" + expression + ")";
    }
}
