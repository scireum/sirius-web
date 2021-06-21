/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.di.std.Register;
import sirius.kernel.health.ExceptionHint;
import sirius.kernel.health.HandledException;

/**
 * Outputs the whole error message while preserving whitespaces.
 * <p>
 * This is mainly used to properly report <b>Tagliatelle</b> errors.
 */
@Register
public class PreserveErrorMessageTransformer implements ErrorMessageTransformer {

    public static final ExceptionHint PRESERVE = new ExceptionHint("preserve");

    @Override
    public String transform(HandledException exception, String message) {
        if (exception.getHint(PRESERVE).asBoolean()) {
            return "<div class=\"text-monospace whitespace-pre-wrap\">" + message + "</div>";
        } else {
            return message;
        }
    }

    @Override
    public int getPriority() {
        return 100;
    }
}
