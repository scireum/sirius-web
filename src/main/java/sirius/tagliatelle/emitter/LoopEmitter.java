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
public class LoopEmitter extends Emitter {

    private Expression iterableExpression;
    private Emitter loop;

    public LoopEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    @Override
    public Emitter copy() {
        LoopEmitter copy = new LoopEmitter(startOfBlock);
        copy.iterableExpression = iterableExpression.copy();
        copy.loop = loop.copy();

        return copy;
    }

    @Override
    public Emitter reduce() {
        this.loop.reduce();

        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        this.loop = visitor.visit(loop);
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(ExpressionVisitor visitor) {
        this.iterableExpression = iterableExpression.visit(visitor);
        this.loop.visitExpressions(visitor);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
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
