/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.emitter;

import sirius.web.templates.engine.RenderContext;
import sirius.web.templates.engine.expression.Expression;

/**
 * Created by aha on 10.05.17.
 */
public class ExpressionEmitter implements Emitter {

    private Expression expression;

    public ExpressionEmitter(Expression expression) {
        this.expression = expression;
    }

    @Override
    public void emit(RenderContext context) {
        Object value = expression.eval(context);
        if (value == null) {
            context.output("");
        } else {
            context.output(value.toString());
        }
    }

}
