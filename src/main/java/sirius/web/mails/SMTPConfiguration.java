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
    private String mailSender;
    private String mailSenderName;
    private boolean useSenderAndEnvelopeFrom;

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
     * Creates a new configuration based on fixed values.
     *
     * @param host                     the mail server host
     * @param port                     the mail server port
     * @param user                     the mail account user
     * @param password                 the mail account password
     * @param mailSender               the sender address
     * @param mailSenderName           the sender name
     * @param useSenderAndEnvelopeFrom whether to fill the "Sender" and the "Envelope-From" header
     */
    public SMTPConfiguration(String host,
                             String port,
                             String user,
                             String password,
                             String mailSender,
                             String mailSenderName,
                             boolean useSenderAndEnvelopeFrom) {
        this.host = host;
        this.port = port;
        this.user = user;
        this.password = password;
        this.mailSender = mailSender;
        this.mailSenderName = mailSenderName;
        this.useSenderAndEnvelopeFrom = useSenderAndEnvelopeFrom;
    }

    /**
     * Creates a new configuration based on the config files.
     *
     * @return a new configuration based on the config files.
     */
    public static SMTPConfiguration fromConfig() {
        return new SMTPConfiguration(smtpHost,
                                     smtpPort,
                                     smtpUser,
                                     smtpPassword,
                                     smtpSender,
                                     smtpSenderName,
                                     smtpUseEnvelopeFrom);
    }

    /**
     * Creates a new configuration based on the current scope and user settings.
     *
     * @return a new configuration based on the current scope and user settings.
     */
    public static SMTPConfiguration fromUserSettings() {
        return fromSettings(UserContext.getSettings());
    }

    /**
     * Creates a new configuration based on the given settings.
     *
     * @param settings the SMTP settings
     * @return a new configuration based on the given settings.
     */
    public static SMTPConfiguration fromSettings(Settings settings) {
        return new SMTPConfiguration(settings.get("mail.host").getString(),
                                     settings.get("mail.port").getString(),
                                     settings.get("mail.user").getString(),
                                     settings.get("mail.password").getString(),
                                     settings.get("mail.sender").getString(),
                                     settings.get("mail.senderName").getString(),
                                     settings.get("mail.useEnvelopeFrom").asBoolean());
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
     * Returns the default value whether a "sender" / "Envelope-From" header should be set
     *
     * @return the default value whether a "sender" / "Envelope-From" header should be set
     */
    public static boolean getDefaultUseSenderAndEnvelopeFrom() {
        return smtpUseEnvelopeFrom;
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
        return useSenderAndEnvelopeFrom;
    }
}
