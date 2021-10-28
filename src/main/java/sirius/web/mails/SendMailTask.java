/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import jakarta.activation.DataHandler;
import jakarta.activation.DataSource;
import jakarta.mail.Authenticator;
import jakarta.mail.Message;
import jakarta.mail.MessagingException;
import jakarta.mail.PasswordAuthentication;
import jakarta.mail.Session;
import jakarta.mail.Transport;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeBodyPart;
import jakarta.mail.internet.MimeMessage;
import jakarta.mail.internet.MimeMultipart;
import net.markenwerk.utils.mail.dkim.Canonicalization;
import net.markenwerk.utils.mail.dkim.DkimMessage;
import net.markenwerk.utils.mail.dkim.DkimSigner;
import net.markenwerk.utils.mail.dkim.SigningAlgorithm;
import sirius.kernel.async.Operation;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Parts;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

/**
 * Contains the effective logic to send a mail in its own task queue.
 */
class SendMailTask implements Runnable {

    private final MailSender mail;
    private final SMTPConfiguration config;
    private boolean success = false;
    private String messageId = null;
    private String technicalSender;
    private String technicalSenderName;

    private static final String X_MAILER = "X-Mailer";
    private static final String MIXED = "mixed";
    private static final String TEXT_HTML_CHARSET_UTF_8 = "text/html; charset=\"UTF-8\"";
    private static final String TEXT_PLAIN_CHARSET_UTF_8 = "text/plain; charset=\"UTF-8\"";
    private static final String CONTENT_TYPE = "Content-Type";
    private static final String MIME_VERSION_1_0 = "1.0";
    private static final String MIME_VERSION = "MIME-Version";
    private static final String ALTERNATIVE = "alternative";
    private static final String MAIL_USER = "mail.user";
    private static final String MAIL_SMTP_AUTH = "mail.smtp.auth";
    private static final String MAIL_TRANSPORT_PROTOCOL = "mail.transport.protocol";
    private static final String MAIL_FROM = "mail.from";
    private static final String MAIL_SMTP_HOST = "mail.smtp.host";
    private static final String MAIL_SMTP_STARTTLS_ENABLE = "mail.smtp.starttls.enable";
    private static final String MAIL_SMTP_SSL_TRUST = "mail.smtp.ssl.trust";
    private static final String MAIL_SMTP_PORT = "mail.smtp.port";
    private static final String MAIL_SMTP_CONNECTIONTIMEOUT = "mail.smtp.connectiontimeout";
    private static final String MAIL_SMTP_TIMEOUT = "mail.smtp.timeout";
    private static final String MAIL_SMTP_WRITETIMEOUT = "mail.smtp.writetimeout";

    /**
     * Defines a header which can be used to add a bounce token to an email.
     * <p>
     * This token can be extracted from received bounce mails and handled properly.
     */
    public static final String X_BOUNCETOKEN = "X-Bouncetoken";

    /*
     * Contains the default timeout used for all socket operations and is set to 60s (=60000ms)
     */
    private static final String MAIL_SOCKET_TIMEOUT = "60000";

    @ConfigValue("mail.smtp.dkim.keyFile")
    private static String dkimKeyFile;

    @ConfigValue("mail.smtp.dkim.domains")
    private static List<String> dkimDomains;
    private static Set<String> dkimDomainSet;

    @ConfigValue("mail.smtp.dkim.selector")
    private static String dkimSelector;
    private static Boolean dkimEnabled;

    @ConfigValue("mail.mailer")
    private static String mailer;

    @Parts(MailLog.class)
    private static Collection<MailLog> logs;

    protected SendMailTask(MailSender mail, SMTPConfiguration config) {
        this.mail = mail;
        this.config = config;
    }

    @Override
    public void run() {
        checkForSimulation();
        determineTechnicalSender();
        try (Operation op = new Operation(() -> "Sending eMail: " + mail.subject + " to: " + mail.receiverEmail,
                                          Duration.ofSeconds(30))) {
            if (!mail.simulate) {
                sendMail();
            } else {
                messageId = "SIMULATED";
                success = true;
            }
        } finally {
            logSentMail();
        }
    }

    private void checkForSimulation() {
        if (mail.receiverEmail != null && mail.receiverEmail.toLowerCase().endsWith(".local")) {
            Mails.LOG.WARN(
                    "Not going to send an email to '%s' with subject '%s' as this is a local address. Going to simulate...",
                    mail.receiverEmail,
                    mail.subject);
            mail.simulate = true;
        }
    }

    private void logSentMail() {
        if (logs.isEmpty()) {
            logToConsole();
        } else {
            notifyMailLogs();
        }
    }

    private void notifyMailLogs() {
        for (MailLog log : logs) {
            try {
                log.logSentMail(success,
                                messageId,
                                Strings.isEmpty(mail.senderEmail) ? technicalSender : mail.senderEmail,
                                Strings.isEmpty(mail.senderEmail) ? technicalSenderName : mail.senderName,
                                mail.receiverEmail,
                                mail.receiverName,
                                mail.subject,
                                mail.text,
                                mail.html,
                                mail.type);
            } catch (Exception e) {
                Exceptions.handle(Mails.LOG, e);
            }
        }
    }

    private void logToConsole() {
        if (success) {
            Mails.LOG.FINE("Sent mail from: '%s' to '%s' with subject: '%s'",
                           Strings.isEmpty(mail.senderEmail) ? technicalSender : mail.senderEmail,
                           mail.receiverEmail,
                           mail.subject);
        } else {
            Mails.LOG.WARN("FAILED to send mail from: '%s' to '%s' with subject: '%s'",
                           Strings.isEmpty(mail.senderEmail) ? technicalSender : mail.senderEmail,
                           mail.receiverEmail,
                           mail.subject);
        }
    }

    private void sendMail() {
        try {
            Mails.LOG.FINE("Sending eMail: " + mail.subject + " to: " + mail.receiverEmail);
            Session session = getMailSession(config);
            try (Transport transport = getSMTPTransport(session, config)) {
                sendMailViaTransport(session, transport);
            }
        } catch (HandledException e) {
            throw e;
        } catch (Exception e) {
            throw Exceptions.handle()
                            .withSystemErrorMessage(
                                    "Invalid mail configuration: %s (Host: %s, Port: %s, User: %s, Password used: %s)",
                                    e.getMessage(),
                                    config.getMailHost(),
                                    config.getMailPort(),
                                    config.getMailUser(),
                                    Strings.isFilled(config.getMailPassword()))
                            .to(Mails.LOG)
                            .error(e)
                            .handle();
        }
    }

    private void sendMailViaTransport(Session session, Transport transport) {
        try {
            MimeMessage msg = signMessage(createMessage(session));

            transport.sendMessage(msg, msg.getAllRecipients());
            messageId = msg.getMessageID();
            success = true;
        } catch (Exception e) {
            throw Exceptions.handle()
                            .withSystemErrorMessage("Cannot send mail to %s from %s with subject '%s': %s (%s)",
                                                    mail.receiverEmail,
                                                    mail.senderEmail,
                                                    mail.subject)
                            .to(Mails.LOG)
                            .error(e)
                            .handle();
        }
    }

    @SuppressWarnings("squid:S1191")
    @Explain("We need the SUN API for DKIM signing.")
    private com.sun.mail.smtp.SMTPMessage createMessage(Session session) throws Exception {
        com.sun.mail.smtp.SMTPMessage msg = new com.sun.mail.smtp.SMTPMessage(session);
        msg.setSubject(mail.subject);
        msg.setRecipients(Message.RecipientType.TO,
                          new InternetAddress[]{new InternetAddress(mail.receiverEmail, mail.receiverName)});
        setupReplyTo(msg);
        setupSender(msg);
        if (Strings.isFilled(mail.html) || !mail.attachments.isEmpty()) {
            MimeMultipart content = createContent(mail.text, mail.html, mail.attachments);
            msg.setContent(content);
            msg.setHeader(CONTENT_TYPE, content.getContentType());
        } else {
            msg.setText(Objects.requireNonNullElse(mail.text, ""));
        }
        msg.setHeader(MIME_VERSION, MIME_VERSION_1_0);
        if (Strings.isFilled(mail.bounceToken)) {
            msg.setHeader(X_BOUNCETOKEN, mail.bounceToken);
        }
        msg.setHeader(X_MAILER, mailer);
        for (Map.Entry<String, String> e : mail.headers.entrySet()) {
            if (Strings.isEmpty(e.getValue())) {
                msg.removeHeader(e.getKey());
            } else {
                msg.setHeader(e.getKey(), e.getValue());
            }
        }
        msg.setSentDate(new Date());
        return msg;
    }

    private static boolean isDkimDomain(String domain) {
        if (dkimDomainSet == null) {
            dkimDomainSet = new TreeSet<>(dkimDomains);
        }

        return dkimDomainSet.contains(domain);
    }

    private static boolean isDkimEnabled() {
        if (dkimEnabled == null) {
            dkimEnabled =
                    Strings.isFilled(dkimSelector) && Strings.isFilled(dkimKeyFile) && new File(dkimKeyFile).exists();
        }

        return dkimEnabled;
    }

    private MimeMessage signMessage(MimeMessage message) {
        if (!isDkimEnabled()) {
            return message;
        }
        String effectiveFrom = mail.senderEmail;
        if (Strings.isEmpty(effectiveFrom)) {
            effectiveFrom = technicalSender;
        }

        String domain = Strings.split(effectiveFrom, "@").getSecond();
        if (!isDkimDomain(domain)) {
            return message;
        }

        try {
            DkimSigner dkimSigner = new DkimSigner(domain, dkimSelector, new File(dkimKeyFile));
            dkimSigner.setIdentity(effectiveFrom);
            dkimSigner.setHeaderCanonicalization(Canonicalization.SIMPLE);
            dkimSigner.setBodyCanonicalization(Canonicalization.RELAXED);
            dkimSigner.setSigningAlgorithm(SigningAlgorithm.SHA256_WITH_RSA);
            dkimSigner.setLengthParam(true);
            dkimSigner.setCopyHeaderFields(false);
            return new DkimMessage(message, dkimSigner);
        } catch (Exception e) {
            Exceptions.handle().to(Mails.LOG).error(e).withNLSKey("Skipping DKIM signing due to: %s (%s)").handle();
        }

        return message;
    }

    private void setupReplyTo(com.sun.mail.smtp.SMTPMessage msg)
            throws UnsupportedEncodingException, MessagingException {
        if (Strings.isFilled(mail.replyToEmail)) {
            if (Strings.isFilled(mail.replyToName)) {
                msg.setReplyTo(new InternetAddress[]{new InternetAddress(mail.replyToEmail, mail.replyToName)});
            } else {
                msg.setReplyTo(new InternetAddress[]{new InternetAddress(mail.replyToEmail)});
            }
        }
    }

    private void setupSender(com.sun.mail.smtp.SMTPMessage msg)
            throws MessagingException, UnsupportedEncodingException {
        if (Strings.isFilled(mail.senderEmail)) {
            if (config.isUseSenderAndEnvelopeFrom()) {
                msg.setSender(new InternetAddress(technicalSender, technicalSenderName));
            }
            msg.setFrom(new InternetAddress(mail.senderEmail, mail.senderName));
        } else {
            msg.setFrom(new InternetAddress(technicalSender, technicalSenderName));
        }
        if (config.isUseSenderAndEnvelopeFrom()) {
            msg.setEnvelopeFrom(Strings.isFilled(config.getMailSender()) ?
                                config.getMailSender() :
                                SMTPConfiguration.getDefaultSender());
        }
    }

    private void determineTechnicalSender() {
        technicalSender = config.getMailSender();
        technicalSenderName = config.getMailSenderName();
        if (Strings.isEmpty(technicalSender)) {
            technicalSender = SMTPConfiguration.getDefaultSender();
            technicalSenderName = SMTPConfiguration.getDefaultSenderName();
        }
    }

    private Session getMailSession(SMTPConfiguration config) {
        Properties props = new Properties();
        props.setProperty(MAIL_SMTP_PORT, Strings.isEmpty(config.getMailPort()) ? "25" : config.getMailPort());
        props.setProperty(MAIL_SMTP_HOST, config.getMailHost());
        if (Strings.isFilled(config.getMailSender())) {
            props.setProperty(MAIL_FROM, config.getMailSender());
        }
        // Set a fixed timeout of 60s for all operations - the default timeout is "infinite"
        props.setProperty(MAIL_SMTP_CONNECTIONTIMEOUT, MAIL_SOCKET_TIMEOUT);
        props.setProperty(MAIL_SMTP_TIMEOUT, MAIL_SOCKET_TIMEOUT);
        props.setProperty(MAIL_SMTP_WRITETIMEOUT, MAIL_SOCKET_TIMEOUT);

        props.setProperty(MAIL_TRANSPORT_PROTOCOL, config.getProtocol().getProtocol());
        props.setProperty(MAIL_SMTP_STARTTLS_ENABLE, Boolean.toString(config.getProtocol().isStarttls()));
        if (Strings.isFilled(config.getTrustedServers())) {
            props.setProperty(MAIL_SMTP_SSL_TRUST, config.getTrustedServers());
        }
        Authenticator auth = new MailAuthenticator(config);
        if (Strings.isEmpty(config.getMailPassword())) {
            props.setProperty(MAIL_SMTP_AUTH, Boolean.FALSE.toString());
            return Session.getInstance(props);
        } else {
            props.setProperty(MAIL_USER, config.getMailUser());
            props.setProperty(MAIL_SMTP_AUTH, Boolean.TRUE.toString());
            return Session.getInstance(props, auth);
        }
    }

    private MimeMultipart createContent(String textPart, String htmlPart, List<DataSource> attachments)
            throws Exception {
        MimeMultipart content = createMainContent(textPart, htmlPart);
        List<DataSource> mixedAttachments = filterAndAppendAlternativeParts(attachments, content);
        if (mixedAttachments.isEmpty()) {
            return content;
        }
        return createMixedContent(content, mixedAttachments);
    }

    /*
     * Adds the text and html body parts as ALTERNATIVE
     */
    private MimeMultipart createMainContent(String textPart, String htmlPart) throws MessagingException {
        MimeMultipart content = new MimeMultipart(ALTERNATIVE);
        MimeBodyPart text = new MimeBodyPart();
        MimeBodyPart html = new MimeBodyPart();
        text.setText(textPart, StandardCharsets.UTF_8.name());
        text.setHeader(MIME_VERSION, MIME_VERSION_1_0);
        text.setHeader(CONTENT_TYPE, TEXT_PLAIN_CHARSET_UTF_8);
        content.addBodyPart(text);
        if (htmlPart != null) {
            html.setText(Strings.replaceUmlautsToHtml(htmlPart), StandardCharsets.UTF_8.name());
            html.setHeader(MIME_VERSION, MIME_VERSION_1_0);
            html.setHeader(CONTENT_TYPE, TEXT_HTML_CHARSET_UTF_8);
            content.addBodyPart(html);
        }
        return content;
    }

    /*
     * Filters all ALTERNATIVE attachments and adds them to the content.
     * returns all "real" attachments which have to be added as mixed body parts
     */
    private List<DataSource> filterAndAppendAlternativeParts(List<DataSource> attachments, MimeMultipart content)
            throws MessagingException {
        // by default an "attachment" would be added as mixed body part
        // however, some attachments like an iCalendar invitation must be added
        // as alternative body part for the given html and text part
        // Therefore we split the attachments into these two categories and then generate
        // the appropriate parts...
        List<DataSource> mixedAttachments = new ArrayList<>();
        for (DataSource attachment : attachments) {
            // Filter null values since var-args are tricky...
            if (attachment != null) {
                if (attachment instanceof Attachment && ((Attachment) attachment).isAlternative()) {
                    MimeBodyPart part = createBodyPart(attachment);
                    content.addBodyPart(part);
                } else {
                    mixedAttachments.add(attachment);
                }
            }
        }
        return mixedAttachments;
    }

    /*
     * Appends all "real" attachments as mixed body parts
     */
    private MimeMultipart createMixedContent(MimeMultipart content, List<DataSource> mixedAttachments)
            throws MessagingException {
        // Generate a new root-multipart which contains the mail-content
        // as alternative-content as well as the attachments.
        MimeMultipart mixed = new MimeMultipart(MIXED);
        MimeBodyPart contentPart = new MimeBodyPart();
        contentPart.setContent(content);
        mixed.addBodyPart(contentPart);
        for (DataSource attachment : mixedAttachments) {
            MimeBodyPart part = createBodyPart(attachment);
            mixed.addBodyPart(part);
        }
        return mixed;
    }

    /*
     * Creates a body part for the given attachment
     */
    private MimeBodyPart createBodyPart(DataSource attachment) throws MessagingException {
        MimeBodyPart part = new MimeBodyPart();
        part.setFileName(attachment.getName());
        part.setDataHandler(new DataHandler(attachment));
        if (attachment instanceof Attachment) {
            for (Map.Entry<String, String> h : ((Attachment) attachment).getHeaders()) {
                if (Strings.isEmpty(h.getValue())) {
                    part.removeHeader(h.getKey());
                } else {
                    part.setHeader(h.getKey(), h.getValue());
                }
            }
        }
        return part;
    }

    private static class MailAuthenticator extends Authenticator {

        private final SMTPConfiguration config;

        private MailAuthenticator(SMTPConfiguration config) {
            this.config = config;
        }

        @Override
        protected PasswordAuthentication getPasswordAuthentication() {
            return new PasswordAuthentication(config.getMailUser(), config.getMailPassword());
        }
    }

    protected Transport getSMTPTransport(Session session, SMTPConfiguration config) {
        try {
            Transport transport = session.getTransport();
            transport.connect(config.getMailHost(), config.getMailUser(), null);
            return transport;
        } catch (Exception e) {
            throw Exceptions.handle()
                            .withSystemErrorMessage(
                                    "Invalid mail configuration: %s (Host: %s, Port: %s, User: %s, Password used: %s)",
                                    e.getMessage(),
                                    config.getMailHost(),
                                    config.getMailPort(),
                                    config.getMailUser(),
                                    Strings.isFilled(config.getMailPassword()))
                            .to(Mails.LOG)
                            .error(e)
                            .handle();
        }
    }
}
