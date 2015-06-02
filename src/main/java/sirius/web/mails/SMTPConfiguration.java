/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

/**
 * Represents a configuration for using a SMTP server.
 */
public interface SMTPConfiguration {
    /**
     * Returns the hostname of the mail server to be used.
     *
     * @return the hostname or ip address of the mail server
     */
    String getMailHost();

    /**
     * Returns the port used to connect to the mail server
     *
     * @return the port used to connect to the mail server (Default for SMTP is 25)
     */
    String getMailPort();

    /**
     * Returns the username used to authenticate against the mail server
     *
     * @return the username used for authentication
     */
    String getMailUser();

    /**
     * Returns the password used to authenticate against the mail server
     *
     * @return the password used for authentication
     */
    String getMailPassword();

    /**
     * Returns the sender address used when no other address is supplied.
     * <p>
     * Also if {@link #isUseSenderAndEnvelopeFrom()} is <tt>true</tt>, this is used as "Sender" as well as
     * the "Envelope-Sender"
     *
     * @return the sender address used if no other address is supplied
     */
    String getMailSender();

    /**
     * Returns the sender name which is used when no other sender name is supplied.
     *
     * @return the name of the sender which is used when no other sender is given
     */
    String getMailSenderName();

    /**
     * Determines if mails sent via this configuration use a "Sender" and "Envelope-From" header with the mail
     * sender / senderName or not.
     *
     * @return <tt>true</tt> if a "sender" / "Envelope-From" header should be set, <tt>false</tt> otherwise
     */
    boolean isUseSenderAndEnvelopeFrom();
}
