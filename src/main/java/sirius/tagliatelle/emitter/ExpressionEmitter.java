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
import sirius.tagliatelle.expression.ExpressionVisitor;

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
    public Emitter copy() {
        return new ExpressionEmitter(startOfBlock, expression.copy());
    }

    @Override
    public Emitter reduce() {
        this.expression = expression.reduce();

        if (expression.isConstant()) {
            Object value = expression.eval(null);
            ConstantEmitter result = new ConstantEmitter(startOfBlock);
            result.append(value == null ? "" : value.toString());

            return result;
        }

        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(ExpressionVisitor visitor) {
        this.expression = visitor.visit(expression);
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
