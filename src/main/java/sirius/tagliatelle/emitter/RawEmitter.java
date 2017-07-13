/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.GlobalRenderContext;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.function.Function;

/**
 * Switches the <tt>escaper</tt> to {@link GlobalRenderContext#escapeRAW(String)} while emitting the given body.
 */
public class RawEmitter extends Emitter {

    private Emitter body;

    /**
     * Creates a new instance for the given position and body.
     *
     * @param startOfBlock the position where the emitter was created
     * @param body         the inner body to output without any escaping
     */
    public RawEmitter(Position startOfBlock, Emitter body) {
        super(startOfBlock);
        this.body = body;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Function<String, String> currentEscaper = context.getGlobalContext().getEscaper();
        context.getGlobalContext().setEscaper(GlobalRenderContext::escapeRAW);
        try {
            body.emit(context);
        } finally {
            context.getGlobalContext().setEscaper(currentEscaper);
        }
    }

    @Override
    public Emitter copy() {
        return new RawEmitter(startOfBlock, body.copy());
    }

    @Override
    public Emitter reduce() {
        if (body != null) {
            body = body.reduce();
        }

        return this;
    }

    @Override
    public Emitter propagateVisitor(EmitterVisitor visitor) {
        if (body != null) {
            this.body = body.propagateVisitor(visitor);
        }
        return visitor.visitThis(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        if (body != null) {
            body.visitExpressions(visitorSupplier);
        }
    }
}
