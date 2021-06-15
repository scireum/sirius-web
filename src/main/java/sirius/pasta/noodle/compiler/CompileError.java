/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import parsii.tokenizer.ParseError;

import javax.annotation.Nullable;

/**
 * Represents an error which occurred during compilation.
 * <p>
 * If possible, it contains the source line, and the exact position along with the error message to assist in
 * fixing the recorded error.
 */
public class CompileError {

    private final ParseError error;
    private final String line;

    /**
     * Creates a new compile error.
     *
     * @param error the underlying parse error which represents the actual problem
     * @param line  the source line used to output a nicely looking and hopefully helpful error message
     */
    public CompileError(ParseError error, @Nullable String line) {
        this.error = error;
        this.line = line;
    }

    /**
     * Returns the underlying parse error.
     *
     * @return the parse error which lead to this compile error
     */
    public ParseError getError() {
        return error;
    }

    /**
     * Returns the line within the source code where the error occurred.
     *
     * @return the source line where the error occurred
     */
    @Nullable
    public String getLine() {
        return line;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append(error);
        if (line != null) {
            result.append("\n   ");
            result.append(line);
            result.append("\n       ");
            for (int i = 0; i < error.getPosition().getPos() - 1; i++) {
                result.append(" ");
            }
            result.append("^\n");
        } else {
            result.append("\n");
        }

        return result.toString();
    }
}
