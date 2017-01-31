/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.sun.mail.smtp.SMTPMessage;
import com.typesafe.config.Config;
import net.markenwerk.utils.mail.dkim.Canonicalization;
import net.markenwerk.utils.mail.dkim.DkimMessage;
import net.markenwerk.utils.mail.dkim.DkimSigner;
import net.markenwerk.utils.mail.dkim.SigningAlgorithm;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Operation;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.Register;
import sirius.kernel.extensions.Extension;
import sirius.kernel.extensions.Extensions;
import sirius.kernel.health.Average;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.Log;
import sirius.kernel.health.metrics.MetricProvider;
import sirius.kernel.health.metrics.MetricsCollector;
import sirius.kernel.nls.NLS;
import sirius.web.http.MimeHelper;
import sirius.web.templates.Templates;
import sirius.web.templates.velocity.VelocityContentHandler;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * Used to send mails using predefined templates.
 */
@Register(classes = {Mails.class, MetricProvider.class})
public class Mails implements MetricProvider {

    /**
     * Contains the logger <tt>mail</tt> used by the mailing framework.
     */
    public static final Log LOG = Log.get("mail");

    /**
     * Defines a header which can be used to add a bounce token to an email.
     * <p>
     * This token can be extracted from received bounce mails and handled properly.
     */
    public static final String X_BOUNCETOKEN = "X-Bouncetoken";

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
    private static final String SMTP = "smtp";
    private static final String MAIL_SMTP_PORT = "mail.smtp.port";
    private static final String MAIL_SMTP_CONNECTIONTIMEOUT = "mail.smtp.connectiontimeout";
    private static final String MAIL_SMTP_TIMEOUT = "mail.smtp.timeout";
    private static final String MAIL_SMTP_WRITETIMEOUT = "mail.smtp.writetimeout";

    /*
     * Contains the default timeout used for all socket operations and is set to 60s (=60000ms)
     */
    private static final String MAIL_SOCKET_TIMEOUT = "60000";

    @ConfigValue("mail.smtp.host")
    private String smtpHost;

    @ConfigValue("mail.smtp.port")
    private int smtpPort;

    @ConfigValue("mail.smtp.user")
    private String smtpUser;

    @ConfigValue("mail.smtp.password")
    private String smtpPassword;

    @ConfigValue("mail.smtp.sender")
    private String smtpSender;

    @ConfigValue("mail.smtp.senderName")
    private String smtpSenderName;

    @ConfigValue("mail.smtp.dkim.keyFile")
    private String dkimKeyFile;

    @ConfigValue("mail.smtp.dkim.domains")
    private List<String> dkimDomains;
    private Set<String> dkimDomainSet;

    @ConfigValue("mail.smtp.dkim.selector")
    private String dkimSelector;

    private Boolean dkimEnabled;

    private final SMTPConfiguration defaultConfig = new DefaultSMTPConfig();

    @ConfigValue("mail.mailer")
    private String mailer;

    @Part
    private Templates templates;

    @Part
    private Tasks tasks;

    @Parts(MailLog.class)
    private Collection<MailLog> logs;

    private Average mailsOut = new Average();

    @Override
    public void gather(MetricsCollector collector) {
        collector.differentialMetric("mails-out", "mails-out", "Mails Sent", mailsOut.getCount(), null);
        collector.metric("mails-duration", "Send Mail Duration", mailsOut.getAndClearAverage(), "ms");
    }

    /**
     * Creates a new builder which is used to specify the mail to send.
     *
     * @return a new builder used to create an email.
     */
    public MailSender createEmail() {
        return new MailSender();
    }

    /**
     * Determines if the given address is a valid eMail address.
     * <p>
     * The <tt>name</tt> is optional and can be left empty. If <tt>address</tt> is null or empty, <tt>false</tt>
     * will be returned.
     *
     * @param address the email address to check
     * @param name    the optional name to also check
     * @return <tt>true</tt> if the given address is valid, <tt>false</tt> otherwise
     */
    public boolean isValidMailAddress(@Nullable String address, @Nullable String name) {
        if (Strings.isEmpty(address)) {
            return false;
        }
        try {
            if (Strings.isFilled(name)) {
                new InternetAddress(address, name).validate();
            } else {
                new InternetAddress(address).validate();
            }
            return true;
        } catch (Throwable e) {
            Exceptions.ignore(e);
            return false;
        }
    }

    /**
     * Determines if the given email address and the optional <tt>name</tt> is valid. Throws a
     * <tt>HandledException</tt> otherwise.
     *
     * @param address the email address to validate
     * @param name    the optional name to validate - can be left empty or <tt>null</tt>
     */
    public void failForInvalidEmail(@Nullable String address, @Nullable String name) {
        if (!isValidMailAddress(address, name)) {
            throw Exceptions.createHandled()
                            .withNLSKey("MailService.invalidAddress")
                            .set("address", Strings.isFilled(name) ? address + " (" + name + ")" : address)
                            .handle();
        }
    }

    /**
     * Used as bridge between the given parameters and JavaMail
     */
    protected class DefaultSMTPConfig implements SMTPConfiguration {

        @Override
        public String getMailHost() {
            return smtpHost;
        }

        @Override
        public String getMailPort() {
            return String.valueOf(smtpPort);
        }

        @Override
        public String getMailUser() {
            return smtpUser;
        }

        @Override
        public String getMailPassword() {
            return smtpPassword;
        }

        @Override
        public String getMailSender() {
            return smtpSender;
        }

        @Override
        public String getMailSenderName() {
            return smtpSenderName;
        }

        @Override
        public boolean isUseSenderAndEnvelopeFrom() {
            return true;
        }
    }

    private static class MailAuthenticator extends Authenticator {

        private SMTPConfiguration config;

        private MailAuthenticator(SMTPConfiguration config) {
            this.config = config;
        }

        @Override
        protected PasswordAuthentication getPasswordAuthentication() {
            return new PasswordAuthentication(config.getMailUser(), config.getMailPassword());
        }
    }

    /**
     * Implements the builder pattern to specify the mail to send.
     */
    public class MailSender {

        protected boolean simulate;
        protected String senderEmail;
        protected String senderName;
        protected String receiverEmail;
        protected String receiverName;
        protected String subject;
        protected String mailExtension;
        protected Context context;
        protected boolean includeHTMLPart = true;
        protected String text;
        protected String html;
        protected List<DataSource> attachments = Lists.newArrayList();
        protected String bounceToken;
        protected String lang;
        protected Map<String, String> headers = Maps.newTreeMap();

        /**
         * Sets the language used to perform {@link sirius.kernel.nls.NLS} lookups when rendering templates.
         *
         * @param langs an array of languages. The first non empty value is used.
         * @return the builder itself
         */
        public MailSender setLang(String... langs) {
            if (langs == null) {
                return this;
            }
            for (String lang : langs) {
                if (Strings.isFilled(lang)) {
                    this.lang = lang;
                    return this;
                }
            }
            return this;
        }

        /**
         * Sets the email address used as sender of the email.
         *
         * @param senderEmail the address used as sender of the email.
         * @return the builder itself
         */
        public MailSender fromEmail(String senderEmail) {
            this.senderEmail = senderEmail;
            return this;
        }

        /**
         * Sets the name used as sender of the mail.
         *
         * @param senderName the senders name of the email
         * @return the builder itself
         */
        public MailSender fromName(String senderName) {
            this.senderName = senderName;
            return this;
        }

        /**
         * Specifies the email address to send the mail to.
         *
         * @param receiverEmail the target address of the email
         * @return the builder itself
         */
        public MailSender toEmail(String receiverEmail) {
            this.receiverEmail = receiverEmail;
            return this;
        }

        /**
         * Specifies the name of the receiver.
         *
         * @param receiverName the name of the receiver
         * @return the builder itself
         */
        public MailSender toName(String receiverName) {
            this.receiverName = receiverName;
            return this;
        }

        /**
         * Specifies the subject line of the mail.
         *
         * @param subject the subject line to use.
         * @return the builder itself
         */
        public MailSender subject(String subject) {
            this.subject = subject;
            return this;
        }

        /**
         * Adds an individual header to the SMTP message.
         *
         * @param name  the name of the header to add
         * @param value the value of the header to add
         * @return the builder itself
         */
        public MailSender addHeader(String name, String value) {
            headers.put(name, value);
            return this;
        }

        /**
         * Specifies the mail template to use.
         * <p>
         * The template is used to fill the subject like as well as the text and HTML part. The <b>mailExtension</b>
         * named here has to be defined in the system config in the mails/template section:
         * <pre>
         * {@code
         * mail {
         *  templates {
         *      my-template {
         *          # optional - set "subject" parameter in the given context instead"
         *          subject = "Velocity expression to describe the subject"
         *
         *          # optional - Language dependent subjects
         *          subject_de = "..."
         *          subject_en = "..."
         *
         *          text = "mail/path-to-the-text-template.vm"
         *          # optional
         *          html = "mail/path-to-the-html-template.vm"
         *
         *          # optional: Language dependent templates:
         *          text_de = "..."
         *          html_de = "..."
         *          text_en = "..."
         *          html_en = "..."
         *
         *           # optional (Add headers to the mail)
         *          headers {
         *            name = value
         *          }
         *
         *          # optional
         *          attachments {
         *              test-pdf {
         *                  template = "mail/test.pdf.vm"
         *                  # optional - evaluated by velocity...
         *                  fileName = "test.pdf"
         *                  # optional
         *                  encoding = "UTF-8"
         *                  # optional
         *                  contentType = "text/plain"
         *                  # optional (treat this attachment as alternative to the body part rather than as
         *                  # "real" attachment)
         *                  alternative = true
         *
         *                  # optional (Add headers to the body part)
         *                  headers {
         *                      name = value
         *                  }
         *
         *              }
         *          }
         *      }
         *  }
         * }
         * }
         * </pre>
         * <p>
         * The given subject line is evaluates by velocity and my therefore either reference variables or use
         * macros like #nls('nls-key'). The given <b>context</b> can be used to pass variables to the templates.
         *
         * @param mailExtension the name of the mail extension to use
         * @param context       the context used to pass in variables used by the templates
         * @return the builder itself
         */
        public MailSender useMailTemplate(String mailExtension, @Nonnull Context context) {
            this.mailExtension = mailExtension;
            this.context = context;
            return this;
        }

        /**
         * Determines whether the HTML part should be included in the email or not. Some email clients have trouble
         * rendering HTML therefore it can be suppressed so that only text emails are sent.
         *
         * @param includeHTMLPart the flag indicating whether the HTML part should be included (default) or not.
         * @return the builder itself
         */
        public MailSender includeHTMLPart(boolean includeHTMLPart) {
            this.includeHTMLPart = includeHTMLPart;
            return this;
        }

        /**
         * Sets the text content of the email.
         *
         * @param text the text content of the email
         * @return the builder itself
         */
        public MailSender textContent(String text) {
            this.text = text;
            return this;
        }

        /**
         * Sets the HTML content of the email.
         *
         * @param html the HTML content of the email
         * @return the builder itself
         */
        public MailSender htmlContent(String html) {
            this.html = html;
            return this;
        }

        /**
         * Adds an attachment to the email.
         *
         * @param attachment the attachment to add to the email
         * @return the builder itself
         */
        public MailSender addAttachment(DataSource attachment) {
            attachments.add(attachment);
            return this;
        }

        /**
         * Adds an array of attachments to the email.
         *
         * @param attachment the attachments to add
         * @return the builder itself
         */
        public MailSender addAttachments(DataSource... attachment) {
            if (attachment != null) {
                attachments.addAll(Arrays.asList(attachment));
            }
            return this;
        }

        /**
         * Adds a list of attachments to the email.
         *
         * @param attachments the attachments to add
         * @return the builder itself
         */
        public MailSender addAttachments(List<DataSource> attachments) {
            if (attachments != null) {
                this.attachments.addAll(attachments);
            }
            return this;
        }

        /**
         * Sets a bounce token.
         * <p>
         * This bounce toke is hopefully included in a bounce email (generated if a mail cannot be delivered).
         * This permits better bounce handling.
         *
         * @param token the token to identify the mail by a bounde handler.
         * @return the builder itself
         */
        public MailSender setBounceToken(String token) {
            this.bounceToken = token;
            return this;
        }

        /**
         * Sets the simulation flag.
         * <p>
         * A mail which is simulated (<tt>simulateOnly</tt> is <tt>true</tt>) will occur
         * in the mail logs etc. but won't actually be sent.
         *
         * @param simulateOnly <tt>true</tt> if the mail should just be simulated but not actually be sent.
         * @return the builder itself
         */
        public MailSender simulate(boolean simulateOnly) {
            this.simulate = simulateOnly;
            return this;
        }

        /**
         * Sends the mail using the given settings.
         * <p>
         * Once all settings are validated, the mail is send in a separate thread so this method will
         * return rather quickly. Note that a {@link sirius.kernel.health.HandledException} is thrown in case
         * of invalid settings (bad mail address etc.).
         */
        public void send() {
            SMTPConfiguration config = new DefaultSMTPConfig();
            String tmpLang = NLS.getCurrentLang();
            try {
                try {
                    if (lang != null) {
                        CallContext.getCurrent().setLang(lang);
                    }
                    fill();
                    sanitize();
                    check();
                    sendMailAsync(config);
                } finally {
                    CallContext.getCurrent().setLang(tmpLang);
                }
            } catch (HandledException e) {
                throw e;
            } catch (Throwable e) {
                throw Exceptions.handle()
                                .withSystemErrorMessage(
                                        "Cannot send mail to '%s (%s)' from '%s (%s)' with subject '%s': %s (%s)",
                                        receiverEmail,
                                        receiverName,
                                        senderEmail,
                                        senderName,
                                        subject)
                                .to(LOG)
                                .error(e)
                                .handle();
            }
        }

        protected void sendMailAsync(SMTPConfiguration config) {
            if (Strings.isEmpty(config.getMailHost())) {
                LOG.WARN("Not going to send an email to '%s' with subject '%s' as no mail server is configured...",
                         receiverEmail,
                         subject);
            } else {
                SendMailTask task = new SendMailTask(this, config);
                tasks.executor("email").fork(task);
            }
        }

        private void check() {
            try {
                if (Strings.isFilled(receiverName)) {
                    new InternetAddress(receiverEmail, receiverName).validate();
                } else {
                    new InternetAddress(receiverEmail).validate();
                }
            } catch (Throwable e) {
                throw Exceptions.handle()
                                .to(LOG)
                                .error(e)
                                .withNLSKey("MailService.invalidReceiver")
                                .set("address",
                                     Strings.isFilled(receiverName) ?
                                     receiverEmail + " (" + receiverName + ")" :
                                     receiverEmail)
                                .handle();
            }

            try {
                if (Strings.isFilled(senderEmail)) {
                    if (Strings.isFilled(senderName)) {
                        new InternetAddress(senderEmail, senderName).validate();
                    } else {
                        new InternetAddress(senderEmail).validate();
                    }
                }
            } catch (Throwable e) {
                throw Exceptions.handle()
                                .to(LOG)
                                .error(e)
                                .withNLSKey("MailService.invalidSender")
                                .set("address",
                                     Strings.isFilled(senderName) ? senderEmail + " (" + senderName + ")" : senderEmail)
                                .handle();
            }
        }

        private void sanitize() {
            if (Strings.isFilled(senderEmail)) {
                senderEmail = senderEmail.replaceAll("\\s", "");
            }
            if (Strings.isFilled(senderName)) {
                senderName = senderName.trim();
            }
            if (Strings.isFilled(receiverEmail)) {
                receiverEmail = receiverEmail.replaceAll("\\s", "");
            }
            if (Strings.isFilled(receiverName)) {
                receiverName = receiverName.trim();
            }
            if (!includeHTMLPart) {
                html = null;
            }
        }

        private void fill() {
            if (Strings.isEmpty(mailExtension)) {
                return;
            }
            Extension ex = findMailExtension();
            context.put("template", mailExtension);
            try {
                fillSubject(ex);
                fillTextContent(ex);
                htmlContent(null);
                if (ex.get("html").isFilled()) {
                    fillHtmlContent(ex);
                }
                if (ex.getConfig("headers") != null) {
                    for (Map.Entry<String, com.typesafe.config.ConfigValue> e : ex.getConfig("headers").entrySet()) {
                        headers.put(e.getKey(), NLS.toMachineString(e.getValue().unwrapped()));
                    }
                }
                generateAttachments(ex);
            } catch (HandledException e) {
                throw e;
            } catch (Throwable e) {
                throw Exceptions.handle()
                                .withSystemErrorMessage(
                                        "Cannot send mail to '%s (%s)' from '%s (%s)' with subject '%s': %s (%s)",
                                        receiverEmail,
                                        receiverName,
                                        senderEmail,
                                        senderName,
                                        subject)
                                .to(LOG)
                                .error(e)
                                .handle();
            }
        }

        private void generateAttachments(Extension ex) {
            for (Config attachmentConfig : ex.getConfigs("attachments")) {
                String template = attachmentConfig.getString("template");
                try {
                    Templates.Generator attachment = templates.generator();
                    if (attachmentConfig.hasPath("encoding")) {
                        attachment.encoding(attachmentConfig.getString("encoding"));
                    }
                    attachment.useTemplate(template);
                    attachment.applyContext(context);
                    ByteArrayOutputStream out = new ByteArrayOutputStream();
                    attachment.generateTo(out);
                    out.flush();
                    String fileName = attachmentConfig.getString("id");
                    if (attachmentConfig.hasPath("fileName")) {
                        fileName = templates.generator()
                                            .direct(attachmentConfig.getString("fileName"), VelocityContentHandler.VM)
                                            .applyContext(context)
                                            .generate();
                    } else {
                        int idx = fileName.lastIndexOf("-");
                        if (idx >= 0) {
                            fileName = fileName.substring(0, idx) + "." + fileName.substring(idx + 1);
                        }
                    }

                    String mimeType = MimeHelper.guessMimeType(fileName);
                    if (attachmentConfig.hasPath("contentType")) {
                        mimeType = attachmentConfig.getString("contentType");
                    }
                    boolean asAlternative = false;
                    if (attachmentConfig.hasPath("alternative")) {
                        asAlternative = attachmentConfig.getBoolean("alternative");
                    }
                    Attachment att = new Attachment(fileName, mimeType, out.toByteArray(), asAlternative);
                    if (attachmentConfig.hasPath("headers")) {
                        for (Map.Entry<String, com.typesafe.config.ConfigValue> e : attachmentConfig.getConfig("headers")
                                                                                                    .entrySet()) {
                            att.addHeader(e.getKey(), NLS.toMachineString(e.getValue().unwrapped()));
                        }
                    }
                    addAttachment(att);
                } catch (Throwable t) {
                    Exceptions.handle()
                              .to(LOG)
                              .error(t)
                              .withSystemErrorMessage("Cannot generate attachment using template %s (%s) "
                                                      + "when sending a mail from '%s' to '%s': %s (%s)",
                                                      mailExtension,
                                                      template,
                                                      senderEmail,
                                                      receiverEmail)
                              .handle();
                }
            }
        }

        private void fillTextContent(Extension ex) {
            textContent(templates.generator()
                                 .useTemplate(ex.get("text_" + NLS.getCurrentLang())
                                                .asString(ex.get("text").asString()))
                                 .applyContext(context)
                                 .generate());
        }

        private void fillSubject(Extension ex) {
            subject(templates.generator()
                             .direct(ex.get("subject_" + NLS.getCurrentLang())
                                       .asString(ex.get("subject").asString("$subject")), VelocityContentHandler.VM)
                             .applyContext(context)
                             .generate());
        }

        private void fillHtmlContent(Extension ex) {
            try {
                htmlContent(templates.generator()
                                     .useTemplate(ex.get("html_" + NLS.getCurrentLang())
                                                    .asString(ex.get("html").asString()))
                                     .applyContext(context)
                                     .generate());
            } catch (Throwable e) {
                Exceptions.handle()
                          .to(LOG)
                          .error(e)
                          .withSystemErrorMessage("Cannot generate HTML content using template %s (%s) "
                                                  + "when sending a mail from '%s' to '%s': %s (%s)",
                                                  mailExtension,
                                                  ex.get("html_" + NLS.getCurrentLang())
                                                    .asString(ex.get("html").asString()),
                                                  senderEmail,
                                                  receiverEmail)
                          .handle();
            }
        }

        private Extension findMailExtension() {
            Extension ex = Extensions.getExtension("mail.templates", mailExtension);
            if (ex == null) {
                throw Exceptions.handle()
                                .withSystemErrorMessage(
                                        "Unknown mail extension: %s. Cannot send mail from: '%s' to '%s'",
                                        mailExtension,
                                        senderEmail,
                                        receiverEmail)
                                .handle();
            }
            return ex;
        }

        public MailSender from(String senderEmail, String senderName) {
            return fromEmail(senderEmail).fromName(senderName);
        }

        public MailSender to(String receiverEmail, String receiverName) {
            return toEmail(receiverEmail).toName(receiverName);
        }
    }

    private class SendMailTask implements Runnable {

        private MailSender mail;
        private SMTPConfiguration config;
        private boolean success = false;
        private String messageId = null;
        private String technicalSender;
        private String technicalSenderName;

        private SendMailTask(MailSender mail, SMTPConfiguration config) {
            this.mail = mail;
            this.config = config;
        }

        @Override
        public void run() {
            if (mail.receiverEmail != null && mail.receiverEmail.toLowerCase().endsWith(".local")) {
                LOG.WARN(
                        "Not going to send an email to '%s' with subject '%s' as this is a local address. Going to simulate...",
                        mail.receiverEmail,
                        mail.subject);
                mail.simulate = true;
            }
            determineTechnicalSender();
            Operation op = Operation.create("mail",
                                            () -> "Sending eMail: " + mail.subject + " to: " + mail.receiverEmail,
                                            Duration.ofSeconds(30));
            try {
                if (!mail.simulate) {
                    sendMail();
                } else {
                    messageId = "SIMULATED";
                    success = true;
                }
            } finally {
                Operation.release(op);
                if (logs.isEmpty()) {
                    if (!success) {
                        LOG.WARN("FAILED to send mail from: '%s' to '%s' with subject: '%s'",
                                 Strings.isEmpty(mail.senderEmail) ? technicalSender : mail.senderEmail,
                                 mail.receiverEmail,
                                 mail.subject);
                    } else {
                        LOG.FINE("Sent mail from: '%s' to '%s' with subject: '%s'",
                                 Strings.isEmpty(mail.senderEmail) ? technicalSender : mail.senderEmail,
                                 mail.receiverEmail,
                                 mail.subject);
                    }
                } else {
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
                                            mail.mailExtension);
                        } catch (Exception e) {
                            Exceptions.handle(LOG, e);
                        }
                    }
                }
            }
        }

        private void sendMail() {
            try {
                LOG.FINE("Sending eMail: " + mail.subject + " to: " + mail.receiverEmail);
                Session session = getMailSession(config);
                Transport transport = getSMTPTransport(session, config);
                try {
                    try {
                        SMTPMessage msg = createMessage(session);

                        transport.sendMessage(msg, msg.getAllRecipients());
                        messageId = msg.getMessageID();
                        success = true;
                    } catch (Throwable e) {
                        throw Exceptions.handle()
                                        .withSystemErrorMessage(
                                                "Cannot send mail to %s from %s with subject '%s': %s (%s)",
                                                mail.receiverEmail,
                                                mail.senderEmail,
                                                mail.subject)
                                        .to(LOG)
                                        .error(e)
                                        .handle();
                    }
                } finally {
                    transport.close();
                }
            } catch (HandledException e) {
                throw e;
            } catch (Throwable e) {
                throw Exceptions.handle()
                                .withSystemErrorMessage(
                                        "Invalid mail configuration: %s (Host: %s, Port: %s, User: %s, Password used: %s)",
                                        e.getMessage(),
                                        config.getMailHost(),
                                        config.getMailPort(),
                                        config.getMailUser(),
                                        Strings.isFilled(config.getMailPassword()))
                                .to(LOG)
                                .error(e)
                                .handle();
            }
        }

        private SMTPMessage createMessage(Session session) throws Exception {
            SMTPMessage msg = new SMTPMessage(session);
            msg.setSubject(mail.subject);
            msg.setRecipients(Message.RecipientType.TO,
                              new InternetAddress[]{new InternetAddress(mail.receiverEmail, mail.receiverName)});
            setupSender(msg);
            if (Strings.isFilled(mail.html) || !mail.attachments.isEmpty()) {
                MimeMultipart content = createContent(mail.text, mail.html, mail.attachments);
                msg.setContent(content);
                msg.setHeader(CONTENT_TYPE, content.getContentType());
            } else {
                if (mail.text != null) {
                    msg.setText(mail.text);
                } else {
                    msg.setText("");
                }
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

        private boolean isDkimDomain(String domain) {
            if (dkimDomainSet == null) {
                dkimDomainSet = Sets.newTreeSet(dkimDomains);
            }

            return dkimDomainSet.contains(domain);
        }

        private boolean isDkimEnabled() {
            if (dkimEnabled == null) {
                dkimEnabled = Strings.isFilled(dkimSelector)
                              && Strings.isFilled(dkimKeyFile)
                              && new File(dkimKeyFile).exists();
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
                dkimSigner.setZParam(false);
                return new DkimMessage(message, dkimSigner);
            } catch (Throwable e) {
                Exceptions.handle().to(LOG).error(e).withNLSKey("Skipping DKIM signing due to: %s (%s)").handle();
            }

            return message;
        }

        private void setupSender(SMTPMessage msg) throws MessagingException, UnsupportedEncodingException {
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
                                    defaultConfig.getMailSender());
            }
        }

        private void determineTechnicalSender() {
            technicalSender = config.getMailSender();
            technicalSenderName = config.getMailSenderName();
            if (Strings.isEmpty(technicalSender)) {
                technicalSender = defaultConfig.getMailSender();
                technicalSenderName = defaultConfig.getMailSenderName();
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

            props.setProperty(MAIL_TRANSPORT_PROTOCOL, SMTP);
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
            text.setText(textPart, Charsets.UTF_8.name());
            text.setHeader(MIME_VERSION, MIME_VERSION_1_0);
            text.setHeader(CONTENT_TYPE, TEXT_PLAIN_CHARSET_UTF_8);
            content.addBodyPart(text);
            if (htmlPart != null) {
                htmlPart = Strings.replaceUmlautsToHtml(htmlPart);
                html.setText(htmlPart, Charsets.UTF_8.name());
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
            List<DataSource> mixedAttachments = Lists.newArrayList();
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
    }

    protected Transport getSMTPTransport(Session session, SMTPConfiguration config) {
        try {
            Transport transport = session.getTransport(SMTP);
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
                            .to(LOG)
                            .error(e)
                            .handle();
        }
    }
}
