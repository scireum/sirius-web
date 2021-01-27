/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

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

    private String value;

    /**
     * Creates a new constant emitter at the given position.
     *
     * @param startOfBlock the position where the emitter was created.
     */
    public ConstantEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    @Override
    public Emitter reduce() {
        return this;
    }

    /**
     * Appends the given string to the output.
     *
     * @param stringToAppend the text to append
     * @return the emitter itself for fluent method calls
     */
    public ConstantEmitter append(String stringToAppend) {
        if (Strings.isEmpty(stringToAppend)) {
            return this;
        }
        if (value == null) {
            value = stringToAppend;
        } else {
            value += stringToAppend;
        }

        return this;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        context.outputRaw(getValue());
    }

    @Override
    public String toString() {
        return getValue();
    }

    /**
     * Returns the underlying text block which will be emitted.
     *
     * @return the text represented by this emitter
     */
    public String getValue() {
        return value == null ? "" : value;
    }
}
