/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.PartCollection;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.Register;

/**
 * Expands help- or error messages.
 * <p>
 * This can be used to expand macros or strings in help or error messages. This is e.g. used by sirius-biz and
 * its knowledge base to easily inject links to articles without having to put HTML into every NLS keys (which is
 * commonly scrambled by translation agencies).
 */
@Register(classes = MessageExpanders.class)
public class MessageExpanders {

    @Parts(MessageExpander.class)
    private PartCollection<MessageExpander> expanders;

    /**
     * Expandes the given message by calling all known {@link MessageExpander} parts.
     *
     * @param message the message to expand
     * @return the expanded message
     */
    public String expand(String message) {
        if (Strings.isEmpty(message)) {
            return message;
        }

        for (MessageExpander expander : expanders) {
            message = expander.expand(message);
        }

        return message;
    }
}
