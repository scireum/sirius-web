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
public class LoopEmitter implements Emitter {

    private Expression iterableExpression;
    private Emitter loop;

    @Override
    public void emit(LocalRenderContext context) {
        Object iterable = iterableExpression.eval(context);
        if (iterable == null) {
            return;
        }

//        for (Object obj : (Iterable<?>) iterable) {
//            context.push(obj);
//            try {
//                loop.emit(context);
//            } finally {
//                context.pop();
//            }
//        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("@for (");
        sb.append(iterableExpression);
        sb.append(") {");
        sb.append(loop);
        sb.append("}");

        return sb.toString();
    }
}
