/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import parsii.tokenizer.Char;
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
    protected int skipWhitespaces() {
        int whitespaceFound = 0;
        while (reader.current().isWhitepace()) {
            reader.consume();
            whitespaceFound++;
        }

        return whitespaceFound;
    }

    /**
     * Skips one or more expected whitespaces.
     * <p>
     * Creates a warning if no or more than one whitsepace was skipped.
     */
    protected void skipExpectedWhitespace() {
        Char current = reader.current();
        if (skipWhitespaces() != 1) {
            context.warning(current, "Consider using a whitespace here.");
        }
    }

    /**
     * Skips all unexpected whitespaces.
     * <p>
     * Creates a warning if whitsepaces were skippd.
     */
    protected void skipUnexpectedWhitespace() {
        Char current = reader.current();
        if (skipWhitespaces() > 0) {
            context.warning(current, "Consider removing this unexpected whitespace here.");
        }
    }

    /**
     * Consumes the expected character from the input.
     * <p>
     * If the input does not point to the given character, nothing will be consumed and en error created.
     *
     * @param expectedCharacter the expected character
     */
    protected void consumeExpectedCharacter(char expectedCharacter) {
        if (!reader.current().is(expectedCharacter)) {
            context.error(reader.current(), "A '%s' was expected here.", expectedCharacter);
        } else {
            reader.consume();
        }
    }
}
