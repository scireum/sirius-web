/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.event;

import sirius.kernel.di.std.AutoRegister;

/**
 * Represents a handler which handles a given URI.
 * <p>
 * This is used as a temporary solution in order to be able to log PageImpressionEvents for the HelpDispatcher.
 */
@AutoRegister
public interface UriHandler {

    /**
     * Handles the given URI.
     *
     * @param uri the URI to handle
     */
    void handle(String uri);
}
