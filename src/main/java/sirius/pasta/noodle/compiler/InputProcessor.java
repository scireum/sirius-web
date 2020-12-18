/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import parsii.tokenizer.Char;
import parsii.tokenizer.LookaheadReader;

/**
 * Provides a base class for all compilers used by the <tt>Pasta</tt> framework.
 */
public class InputProcessor {

    protected LookaheadReader reader;
    protected CompilationContext context;

    /**
     * Creates a new instance which operates on the given input and uses the given context.
     *
     * @param context the context used during compilation
     * @param reader  the reader pointing a the expression to parse
     */
    protected InputProcessor(CompilationContext context, LookaheadReader reader) {
        this.reader = reader;
        this.context = context;
    }

    /**
     * Skips all whitespace characters at the current input location.
     *
     * @return the number of whitespaces which were skipped
     */
    public int skipWhitespaces() {
        int whitespaceFound = 0;
        while (isAtWhitespace()) {
            Char current = reader.consume();
            if (current.is('\u00A0')) {
                context.warning(current,
                                "A non-breaking whitespace (Ux00A0) was found! "
                                + "This looks like an innocent whitespace but isn't and may break many systems.");
            }
            whitespaceFound++;
        }

        return whitespaceFound;
    }

    /**
     * Determines if we're currently looking at a whitespace.
     *
     * @return <tt>true</tt> if the current char we're looking at is a whitespace, <tt>false</tt> otherwise
     */
    protected boolean isAtWhitespace() {
        return reader.current().isWhitepace() || reader.current().is('\u00A0');
    }

    /**
     * Consumes the expected character from the input.
     * <p>
     * If the input does not point to the given character, nothing will be consumed and en error created.
     *
     * @param expectedCharacter the expected character
     * @return <tt>true</tt> if the character was read, <tt>false</tt> otherwise
     */
    public boolean consumeExpectedCharacter(char expectedCharacter) {
        if (!reader.current().is(expectedCharacter)) {
            context.error(reader.current(), "A '%s' was expected here.", expectedCharacter);
            return false;
        } else {
            reader.consume();
            return true;
        }
    }

    /**
     * Determines if the parser is currently at the given text / keyword.
     *
     * @param offset the offset to add to the parsers position
     * @param text   the text or keyword to check for
     * @return <tt>true</tt> if the parser is currently at the given text, <tt>false</tt> otherwise
     */
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
        return getClass().getSimpleName() + ": " + reader;
    }
}
