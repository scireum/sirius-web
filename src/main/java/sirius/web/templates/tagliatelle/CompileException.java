/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle;

import java.util.List;

/**
 * Created by aha on 12.05.17.
 */
public class CompileException extends Exception {

    private List<CompileError> errors;

    /**
     * Creates a new exception based on the list of errors.
     *
     * @param errors the errors which occurred while processing the user input
     * @return a new ParseException which can be thrown
     */
    public static CompileException create(List<CompileError> errors) {
        if (errors.size() == 1) {
            return new CompileException(errors.get(0).getError().getMessage(), errors);
        } else if (errors.size() > 1) {
            return new CompileException(String.format("%d errors occured. First: %s",
                                                      errors.size(),
                                                      errors.get(0).getError().getMessage()), errors);
        } else {
            return new CompileException("An unknown error occured", errors);
        }
    }

    private CompileException(String message, List<CompileError> errors) {
        super(message);
        this.errors = errors;
    }

    /**
     * Provides a list of all errors and warnings which occurred
     *
     * @return all errors and warnings which occurred while processing the user input
     */
    public List<CompileError> getErrors() {
        return errors;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (CompileError error : errors) {
            if (sb.length() > 0) {
                sb.append("\n");
            }
            sb.append(error);
        }

        return sb.toString();
    }
}
