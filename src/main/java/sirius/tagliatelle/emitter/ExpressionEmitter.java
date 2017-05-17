/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.LocalRenderContext;
import sirius.tagliatelle.expression.Expression;

/**
 * Created by aha on 10.05.17.
 */
public class ExpressionEmitter extends Emitter {

    private Expression expression;

    public ExpressionEmitter(Position startOfBlock, Expression expression) {
        super(startOfBlock);
        this.expression = expression;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
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
