/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.web.controller.Message;

import java.util.function.Consumer;

/**
 * Permits to add generally displayed messages.
 * <p>
 * {@link ScopeInfo} and {@link UserInfo} can be transformed to this class to obtain additional messages to show.
 */
public interface MessageProvider {

    /**
     * Permits to add additional messages when rendering a page.
     *
     * @param messageConsumer the consumer to supply messages to
     */
    void addMessages(Consumer<Message> messageConsumer);
}
