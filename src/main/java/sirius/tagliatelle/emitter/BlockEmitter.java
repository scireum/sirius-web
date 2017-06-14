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
 * Invokes a given parameter block.
 * <p>
 * Template blocks can be defined using <tt>&lt;i:block&gt;</tt>. These can be passed to a tag and invoked there. This
 * invokation is represented by this emitter. If no parameter value was given, the tag body (which is the
 * <tt>alternative</tt>) is evaluated.
 */
public class BlockEmitter extends Emitter {

    private String name;
    private Emitter alternative;

    /**
     * Creates a new emitter which outputs the given block parameter at runtime or evaluates the given alternative.
     *
     * @param startOfBlock the position where the block was defined
     * @param name         the name of the block to evaluate
     * @param alternative  the alternative to evaluate if no block is given
     * @see sirius.tagliatelle.tags.TagBlock
     */
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

    @Override
    public Emitter copy() {
        return new BlockEmitter(startOfBlock, name, alternative == null ? null : alternative.copy());
    }

    @Override
    public Emitter reduce() {
        if (alternative != null) {
            alternative = alternative.reduce();
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

    /**
     * Returns the name of the block being referenced.
     *
     * @return the name of the block to output
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the alternative to evaluate if no block with the given name is available.
     *
     * @return the alternative to evaluate
     */
    public Emitter getAlternative() {
        return alternative;
    }
}
