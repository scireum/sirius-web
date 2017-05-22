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
import sirius.tagliatelle.RenderException;
import sirius.tagliatelle.expression.ExpressionVisitor;

/**
 * Created by aha on 10.05.17.
 */
public abstract class Emitter {

    protected Position startOfBlock;

    public Emitter(Position startOfBlock) {
        this.startOfBlock = startOfBlock;
    }

    public void emit(LocalRenderContext context) throws RenderException {
        context.updatePosition(startOfBlock);
        try {
            emitToContext(context);
        } catch (Exception ex) {
            throw RenderException.create(context, ex);
        }
    }

    public Position getStartOfBlock() {
        return startOfBlock;
    }

    protected abstract void emitToContext(LocalRenderContext context) throws Exception;

    public abstract Emitter copy();

    public abstract Emitter reduce();

    public abstract Emitter visit(EmitterVisitor visitor);

    public abstract void visitExpressions(ExpressionVisitor visitor);
}
