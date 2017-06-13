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
 * Emits a constant text block.
 */
public class ConstantEmitter extends Emitter {

    /**
     * Represents an empty emitter which does not contribute the the output.
     */
    public static final ConstantEmitter EMPTY = new ConstantEmitter(Position.UNKNOWN) {
        @Override
        public ConstantEmitter append(String stringToAppend) {
            throw new UnsupportedOperationException();
        }
    };

    private String value = "";

    /**
     * Creates a new constant emitter at the given position.
     *
     * @param startOfBlock the position where the emitter was created.
     */
    public ConstantEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    /**
     * Creates a copy of the emitter.
     * <p>
     * Although the emitter is constant, we still need to create a copy, as the {@link CompositeEmitter} folds adjacent
     * constant blocks into one.
     *
     * @return a copy of this emitter
     */
    @Override
    public Emitter copy() {
        return new ConstantEmitter(startOfBlock).append(value);
    }

    @Override
    public Emitter reduce() {
        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        // No internal expressions
    }

    /**
     * Appends the given string to the output.
     *
     * @param stringToAppend the text to append
     * @return the emitter itself for fluent method calls
     */
    public ConstantEmitter append(String stringToAppend) {
        value += stringToAppend;
        return this;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        context.outputRaw(value);
    }

    @Override
    public String toString() {
        return value;
    }

    /**
     * Returns the underlying text block which will be emitted.
     *
     * @return the text represented by this emitter
     */
    public String getValue() {
        return value;
    }
}
