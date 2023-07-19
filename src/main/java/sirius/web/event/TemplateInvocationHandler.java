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
 * Represents a handler which handles template invocations.
 * <p>
 * This is used as a temporary solution in order to be able to log PageImpressionEvents for the HelpDispatcher.
 */
@AutoRegister
public interface TemplateInvocationHandler {

    /**
     * Handles a template invocation which has been invoked via uri.
     *
     * @param uri the URI to handle
     * @param templateExists <tt>true</tt> if a template exists for the given URI, <tt>false</tt> otherwise
     */
    void handleUriInvocation(String uri, boolean templateExists);
}
