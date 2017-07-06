/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.settings.Settings;
import sirius.web.security.UserContext;

/**
 * Represents a configuration for using a SMTP server.
 */
public class SMTPConfiguration {

    private String host;
    private String port;
    private String user;
    private String password;

    private boolean useSenderAndEnvelopreFrom;

    private String mailSender;
    private String mailSenderName;

    @ConfigValue("mail.smtp.host")
    private static String smtpHost;

    @ConfigValue("mail.smtp.port")
    private static String smtpPort;

    @ConfigValue("mail.smtp.user")
    private static String smtpUser;

    @ConfigValue("mail.smtp.password")
    private static String smtpPassword;

    @ConfigValue("mail.smtp.sender")
    private static String smtpSender;

    @ConfigValue("mail.smtp.senderName")
    private static String smtpSenderName;

    @ConfigValue("mail.smtp.useEnvelopeFrom")
    private static boolean smtpUseEnvelopeFrom;

    /**
     * Creates a new configuration based on either the system configuration or the current scope.
     * <p>
     * Therefore such an instance must not be cached or reused.
     */
    public SMTPConfiguration() {
        Settings settings = UserContext.getSettings();
        if (settings.get("mail.host").isFilled()) {
            this.host = settings.get("mail.host").getString();
            this.port = settings.get("mail.port").getString();
            this.user = settings.get("mail.user").getString();
            this.password = settings.get("mail.password").getString();
        } else {
            this.host = smtpHost;
            this.port = smtpPort;
            this.user = smtpUser;
            this.password = smtpPassword;
        }

        this.mailSender = settings.get("mail.sender").asString(smtpSender);
        this.mailSenderName = settings.get("mail.senderName").asString(smtpSenderName);
        this.useSenderAndEnvelopreFrom = settings.get("mail.useEnvelopeFrom").asBoolean(smtpUseEnvelopeFrom);
    }

    /**
     * Returns the default sender from the system configuration.
     *
     * @return the email address used as sender
     */
    public static String getDefaultSender() {
        return smtpSender;
    }

    /**
     * Returns the default sender name from the system configuration.
     *
     * @return the name used as sender
     */
    public static String getDefaultSenderName() {
        return smtpSenderName;
    }

    /**
     * Returns the hostname of the mail server to be used.
     *
     * @return the hostname or ip address of the mail server
     */
    public String getMailHost() {
        return host;
    }

    /**
     * Returns the port used to connect to the mail server
     *
     * @return the port used to connect to the mail server (Default for SMTP is 25)
     */
    public String getMailPort() {
        return port;
    }

    /**
     * Returns the username used to authenticate against the mail server
     *
     * @return the username used for authentication
     */
    public String getMailUser() {
        return user;
    }

    /**
     * Returns the password used to authenticate against the mail server
     *
     * @return the password used for authentication
     */
    public String getMailPassword() {
        return password;
    }

    /**
     * Returns the sender address used when no other address is supplied.
     * <p>
     * Also if {@link #isUseSenderAndEnvelopeFrom()} is <tt>true</tt>, this is used as "Sender" as well as
     * the "Envelope-Sender"
     *
     * @return the sender address used if no other address is supplied
     */
    public String getMailSender() {
        return mailSender;
    }

    /**
     * Returns the sender name which is used when no other sender name is supplied.
     *
     * @return the name of the sender which is used when no other sender is given
     */
    public String getMailSenderName() {
        return mailSenderName;
    }

    /**
     * Determines if mails sent via this configuration use a "Sender" and "Envelope-From" header with the mail
     * sender / senderName or not.
     *
     * @return <tt>true</tt> if a "sender" / "Envelope-From" header should be set, <tt>false</tt> otherwise
     */
    public boolean isUseSenderAndEnvelopeFrom() {
        return useSenderAndEnvelopreFrom;
    }
}
