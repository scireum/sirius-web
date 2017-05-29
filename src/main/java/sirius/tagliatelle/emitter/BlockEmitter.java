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
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.function.Function;

/**
 * Created by aha on 16.05.17.
 */
public class BlockEmitter extends Emitter {

    private String name;
    private Emitter alternative;

    public BlockEmitter(Position startOfBlock, String name, Emitter alternative) {
        super(startOfBlock);
        this.name = name;
        this.alternative = alternative;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        if (!context.emitBlock(name)) {
            if (alternative != null) {
                alternative.emit(context);
            }
        }
    }

    public String getName() {
        return name;
    }

    @Override
    public Emitter copy() {
        return new BlockEmitter(startOfBlock, name, alternative == null ? null : alternative.copy());
    }

    @Override
    public Emitter reduce() {
        if (alternative != null) {
            alternative.reduce();
        }
        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        if (alternative != null) {
            this.alternative = visitor.visit(alternative);
        }
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        if (alternative != null) {
            alternative.visitExpressions(visitorSupplier);
        }
    }

    public Emitter getAlternative() {
        return alternative;
    }
}
