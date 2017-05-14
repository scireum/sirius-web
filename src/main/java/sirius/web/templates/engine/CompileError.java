/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import parsii.tokenizer.ParseError;

/**
 * Created by aha on 12.05.17.
 */
public class CompileError {
    private ParseError error;
    private String line;

    public CompileError(ParseError error, String line) {
        this.error = error;
        this.line = line;
    }

    public ParseError getError() {
        return error;
    }

    public String getLine() {
        return line;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(error);
        if (line != null) {
            sb.append("\n   ");
            sb.append(line);
            sb.append("\n   ");
            for (int i = 0; i < error.getPosition().getPos() - 1; i++) {
                sb.append(" ");
            }
            sb.append("^\n");
        } else {
            sb.append("\n");
        }

        return sb.toString();
    }
}
