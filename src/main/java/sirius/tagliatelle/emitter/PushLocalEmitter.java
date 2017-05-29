/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.function.Function;

/**
 * Created by aha on 10.05.17.
 */
public class PushLocalEmitter extends Emitter {

    private final int localIndex;
    private Expression expression;

    public PushLocalEmitter(Position startOfBlock, int localIndex, Expression expression) {
        super(startOfBlock);
        this.localIndex = localIndex;
        this.expression = expression;
    }

    @Override
    public Emitter copy() {
        return new PushLocalEmitter(startOfBlock, localIndex, expression.copy());
    }

    @Override
    public Emitter reduce() {
        this.expression = expression.reduce();

        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        ExpressionVisitor visitor = visitorSupplier.apply(getStartOfBlock());
        this.expression = expression.visit(visitor);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Object value = expression.eval(context);
        context.setLocal(localIndex, value);
    }

    @Override
    public String toString() {
        return "LOCAL<" + localIndex + "> = @(" + expression + ")";
    }
}
