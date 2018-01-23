/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import sirius.kernel.commons.Explain;

/**
 * Implementations of this interface can be registered in the component model and will be notified once a mail is tried
 * to be sent.
 */
public interface MailLog {

    /**
     * Invoked once a mail was tried to be sent.
     *
     * @param success      flag indicating if the mail was successfully sent
     * @param messageId    id of the message assigned by Javamail
     * @param sender       email address of the sender
     * @param senderName   name of the sender
     * @param receiver     email address of the receiver
     * @param receiverName name of the receiver
     * @param subject      subject line of the mail
     * @param text         text part of the mail
     * @param html         html part of the mail
     * @param type         type of the mail
     */
    @SuppressWarnings("squid:S00107")
    @Explain("Refactoring into a parameter object would be the way to go but quite an overhead for a simple logging API")
    void logSentMail(boolean success,
                     String messageId,
                     String sender,
                     String senderName,
                     String receiver,
                     String receiverName,
                     String subject,
                     String text,
                     String html,
                     String type);
}
