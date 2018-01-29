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

import javax.annotation.Nonnull;
import java.util.function.Function;

/**
 * Emits the contents of an i:block defined at top-level into an extra block output of the context.
 * <p>
 * This permits to render several strings and sections within a single template.
 *
 * @see sirius.tagliatelle.rendering.GlobalRenderContext#getExtraBlock(String)
 */
public class ExtraBlockEmitter extends Emitter {

    private final String name;
    private Emitter body;

    /**
     * Creates a new instance with the given name and body.
     *
     * @param name the name of the block
     * @param body the body of the block
     */
    public ExtraBlockEmitter(String name, Emitter body) {
        super(body.startOfBlock);
        this.name = name;
        this.body = body;
    }

    @Override
    protected void emitToContext(@Nonnull LocalRenderContext context) throws Exception {
        context.getGlobalContext().beginExtraBlock();
        try {
            body.emit(context);
        } finally {
            context.getGlobalContext().completeExtraBlock(name);
        }
    }

    @Nonnull
    @Override
    public Emitter copy() {
        return new ExtraBlockEmitter(name, body.copy());
    }

    @Nonnull
    @Override
    public Emitter reduce() {
        body = body.reduce();
        return this;
    }

    @Nonnull
    @Override
    public Emitter propagateVisitor(@Nonnull EmitterVisitor visitor) {
        body = body.propagateVisitor(visitor);
        return visitor.visitThis(this);
    }

    @Override
    public void visitExpressions(@Nonnull Function<Position, ExpressionVisitor> visitor) {
        body.visitExpressions(visitor);
    }
}
