/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import parsii.tokenizer.LookaheadReader;

/**
 * Contains methods common to {@link Compiler} and {@link Parser}.
 */
class InputProcessor {
    protected LookaheadReader reader;
    protected CompilationContext context;

    /**
     * Creates a new instance which operates on the given input and uses the given context.
     *
     * @param input   the input to process
     * @param context the context used during compilation
     */
    protected InputProcessor(LookaheadReader input, CompilationContext context) {
        this.reader = input;
        this.context = context;
    }

    /**
     * Skips all whitespace characters at the current input location.
     *
     * @return the number of whitespaces which were skipped
     */
    public int skipWhitespaces() {
        int whitespaceFound = 0;
        while (reader.current().isWhitepace()) {
            reader.consume();
            whitespaceFound++;
        }

        return whitespaceFound;
    }

    /**
     * Consumes the expected character from the input.
     * <p>
     * If the input does not point to the given character, nothing will be consumed and en error created.
     *
     * @param expectedCharacter the expected character
     */
    public void consumeExpectedCharacter(char expectedCharacter) {
        if (!reader.current().is(expectedCharacter)) {
            context.error(reader.current(), "A '%s' was expected here.", expectedCharacter);
        } else {
            reader.consume();
        }
    }

    public boolean isAtText(int offset, String text) {
        for (int i = 0; i < text.length(); i++) {
            if (!reader.next(offset + i).is(text.charAt(i))) {
                return false;
            }
        }

        return true;
    }

    public LookaheadReader getReader() {
        return reader;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + ": " + reader + " - " + context.getTemplate();
    }
}
