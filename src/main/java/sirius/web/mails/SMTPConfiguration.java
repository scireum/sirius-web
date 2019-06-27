/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import sirius.kernel.Sirius;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.nls.NLS;
import sirius.kernel.settings.Settings;
import sirius.web.security.UserContext;

/**
 * Represents a configuration for using a SMTP server.
 */
public class SMTPConfiguration {

    private String host;
    private String port;
    private SMTPProtocol protocol;
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

    private SMTPConfiguration() {
    }

    /**
     * Creates a new {@link SMTPConfiguration} which can be used to configure
     * the mail server to use.
     *
     * @return a {@link SMTPConfiguration} to fill with the settings of the mail
     * server to use
     */
    public static SMTPConfiguration create() {
        return new SMTPConfiguration();
    }

    public SMTPConfiguration setHost(String host) {
        this.host = host;
        return this;
    }

    public SMTPConfiguration setPort(String port) {
        this.port = port;
        return this;
    }

    public SMTPConfiguration setProtocol(SMTPProtocol protocol) {
        this.protocol = protocol;
        return this;
    }

    public SMTPConfiguration setUser(String user) {
        this.user = user;
        return this;
    }

    public SMTPConfiguration setPassword(String password) {
        this.password = password;
        return this;
    }

    public SMTPConfiguration setMailSender(String mailSender) {
        this.mailSender = mailSender;
        return this;
    }

    public SMTPConfiguration setMailSenderName(String mailSenderName) {
        this.mailSenderName = mailSenderName;
        return this;
    }

    public SMTPConfiguration setUseSenderAndEnvelopeFrom(boolean useSenderAndEnvelopeFrom) {
        this.useSenderAndEnvelopeFrom = useSenderAndEnvelopeFrom;
        return this;
    }

    /**
     * Creates a new configuration based on the config files.
     *
     * @return a new configuration based on the config files.
     */
    public static SMTPConfiguration fromConfig() {
        return SMTPConfiguration.create()
                                .setHost(smtpHost)
                                .setPort(smtpPort)
                                .setProtocol(asSMTPProtocol(Sirius.getSettings().get("mail.smtp.protocol")))
                                .setUser(smtpUser)
                                .setPassword(smtpPassword)
                                .setMailSender(smtpSender)
                                .setMailSenderName(smtpSenderName)
                                .setUseSenderAndEnvelopeFrom(smtpUseEnvelopeFrom);
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
        return SMTPConfiguration.create()
                                .setHost(settings.get("mail.host").getString())
                                .setPort(settings.get("mail.port").getString())
                                .setProtocol(asSMTPProtocol(settings.get("mail.protocol")))
                                .setUser(settings.get("mail.user").getString())
                                .setPassword(settings.get("mail.password").getString())
                                .setMailSender(settings.get("mail.sender").getString())
                                .setMailSenderName(settings.get("mail.senderName").getString())
                                .setUseSenderAndEnvelopeFrom(settings.get("mail.useEnvelopeFrom").asBoolean());
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
     * Returns the protocol used to connect to the mail server (which might be encrypted)
     *
     * @return the protocol used to connect to the mail server (which might be encrypted)
     */
    public SMTPProtocol getProtocol() {
        return protocol;
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

    private static SMTPProtocol asSMTPProtocol(Value setting) {
        return setting.getEnum(SMTPProtocol.class)
                      .orElseGet(() -> Value.of(setting.toUpperCase())
                                            .getEnum(SMTPProtocol.class)
                                            .orElse(SMTPProtocol.SMTP));
    }

    /**
     * Lists protocols and encryption methods for SMTP.
     */
    public enum SMTPProtocol {

        /**
         * SMTP over an unencrypted connection
         */
        SMTP("smtp", false),

        /**
         * SMTP over a connection that is encrypted on transport layer (TLS)
         */
        SMTPS("smtps", false),

        /**
         * SMTP over a connection that is encrypted with STARTTLS
         */
        STARTTLS("smtp", true);

        private final String protocol;
        private final boolean starttls;

        SMTPProtocol(String protocol, boolean starttls) {
            this.protocol = protocol;
            this.starttls = starttls;
        }

        public String getProtocol() {
            return protocol;
        }

        public boolean isStarttls() {
            return starttls;
        }

        /**
         * Determines if the protocol supports encryption.
         *
         * @return <tt>true</tt> if the protocol supports encryption, <tt>false</tt> otherwise
         */
        public boolean supportsEncryption() {
            return starttls || "smtps".equals(protocol);
        }

        @Override
        public String toString() {
            return NLS.get(SMTPProtocol.class.getSimpleName() + "." + name());
        }
    }
}
