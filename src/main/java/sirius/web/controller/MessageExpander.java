/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

/**
 * Permits intercepting and expanding messages which invoke the {@link MessageExpanders}.
 *
 * @see MessageExpanders
 */
public interface MessageExpander {

    /**
     * Expands the given message.
     *
     * @param message the message to expand
     * @return the expanded message
     */
    String expand(String message);
}
