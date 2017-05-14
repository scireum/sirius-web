/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import parsii.tokenizer.Char;
import parsii.tokenizer.LookaheadReader;

/**
 * Created by aha on 12.05.17.
 */
public class InputProcessor {
    protected LookaheadReader reader;
    protected CompilationContext context;

    public InputProcessor(LookaheadReader input, CompilationContext context) {
        this.reader = input;
        this.context = context;
    }

    protected int skipWhitespaces() {
        int whitespaceFound = 0;
        while (reader.current().isWhitepace()) {
            reader.consume();
            whitespaceFound++;
        }

        return whitespaceFound;
    }

    protected void skipExpectedWhitespace() {
        Char current = reader.current();
        if (skipWhitespaces() > 1) {
            context.warning(current, "Consider using a whitespace here.");
        }
    }

    protected void skipUnexpectedWhitespace() {
        Char current = reader.current();
        if (skipWhitespaces() > 0) {
            context.warning(current, "Consider removing this unexpected whitespace here.");
        }
    }

    protected void consumeExpectedCharacter(char c) {
        if (!reader.current().is(c)) {
            context.error(reader.current(), "A '%s' was expected here.", c);
        } else {
            reader.consume();
        }
    }
}
