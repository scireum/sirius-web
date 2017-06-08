/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

/**
 * Thrown if an unexpected {@link Exception} is thrown when evaluating an expression.
 */
public class ExpressionEvaluationException extends RuntimeException {

    private static final long serialVersionUID = -8716567016454473447L;

    /**
     * Creates a new instance with the given cause
     *
     * @param cause the exception which occurred.
     */
    public ExpressionEvaluationException(Throwable cause) {
        super(cause);
    }
}
