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
public class LoopEmitter implements Emitter {

    private Expression iterableExpression;
    private Emitter loop;

    @Override
    public void emit(RenderContext context) {
        Object iterable = iterableExpression.eval(context);
        if (iterable == null) {
            return;
        }

        for(Object obj : (Iterable<?>)iterable) {
            context.push(obj);
            try {
                loop.emit(context);
            } finally {
                context.pop();
            }
        }
    }
}
