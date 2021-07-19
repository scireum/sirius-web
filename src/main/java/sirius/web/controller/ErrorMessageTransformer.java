/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.di.std.AutoRegister;
import sirius.kernel.di.std.Priorized;
import sirius.kernel.health.HandledException;

/**
 * Can be used to further process or enhance error messages.
 *
 * @see Message#error(Throwable)
 */
@AutoRegister
public interface ErrorMessageTransformer extends Priorized {

    /**
     * Transforms the given message based on the hints in the given exception.
     *
     * @param exception the actual error which is being processed.
     * @param message   the HTML error message which has been generated so far
     * @return the enhanced message. If no enhancements are available, the original message will be returned
     */
    String transform(HandledException exception, String message);
}
