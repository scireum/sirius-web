/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import jakarta.activation.DataSource;
import jakarta.mail.internet.InternetAddress;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.web.http.MimeHelper;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.security.UserContext;
import sirius.web.templates.Generator;
import sirius.web.templates.Templates;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.net.IDN;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * Implements the builder pattern to specify the mail to send.
 */
public class MailSender {

    protected static final String CONFIG_KEY_HEADERS = "headers";
    protected boolean simulate;
    protected SMTPConfiguration smtpConfiguration;
    protected String senderEmail;
    protected String senderName;
    protected String receiverEmail;
    protected String receiverName;
    protected String replyToEmail;
    protected String replyToName;
    protected String subject;
    protected String subjectKey;
    protected Map<String, Object> subjectParams;
    protected Context textContext;
    protected Context htmlContext;
    protected String textTemplate;
    protected String htmlTemplate;
    protected String text;
    protected String html;
    protected String type;
    protected List<DataSource> attachments = new ArrayList<>();
    protected String bounceToken;
    protected String lang;
    protected Map<String, String> headers = new TreeMap<>();

    @Part
    private static Mails mails;

    @Part
    private static Resources resources;

    @Part
    private static Tasks tasks;

    @Part
    private static Templates templates;

    protected MailSender() {
    }

    /**
     * Sets a custom SMTP configuration.
     *
     * @param smtpConfiguration the custom SMTP configuration
     * @return the builder itself
     */
    public MailSender withSMTPConfiguration(SMTPConfiguration smtpConfiguration) {
        this.smtpConfiguration = smtpConfiguration;
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
     * Specifies both, the sender email and name.
     * <p>
     * This is boilerplate for {@code fromEmail(senderEmail).fromName(senderName)}
     *
     * @param senderEmail the email address which sent the email
     * @param senderName  the name of the sender of the email
     * @return the builder itself
     */
    public MailSender from(String senderEmail, String senderName) {
        return fromEmail(senderEmail).fromName(senderName);
    }

    /**
     * Specifies both, the receiver email and name.
     * <p>
     * This is boilerplate for {@code toEmail(receiverEmail).toName(receiverName)}
     *
     * @param receiverEmail the email address which should receive the email
     * @param receiverName  the name of the receiver of the email
     * @return the builder itself
     */
    public MailSender to(String receiverEmail, String receiverName) {
        return toEmail(receiverEmail).toName(receiverName);
    }

    /**
     * Specifies both, the reply-to email and name.
     *
     * @param replyToEmail the email address which should be used as reply-to
     * @param replyToName  the name of the reply-to address
     * @return the builder itself
     */
    public MailSender replyTo(String replyToEmail, String replyToName) {
        this.replyToEmail = replyToEmail;
        this.replyToName = replyToName;
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
     * Specifies the NLS key used to generate the subject line of the mail.
     * This uses the language specified in {@link #setLang(String...)} if provided
     *
     * @param subjectKey the NLS key for the subject line to use.
     * @return the builder itself
     */
    public MailSender nlsSubject(String subjectKey) {
        return nlsSubject(subjectKey, null);
    }

    /**
     * Specifies the NLS key and optional parameters used to generate the subject line of the mail.
     * This uses the language specified in {@link #setLang(String...)} if provided
     *
     * @param subjectKey    the NLS key for the subject line to use.
     * @param subjectParams the parameters used to format the subject line
     * @return the builder itself
     */
    public MailSender nlsSubject(String subjectKey, @Nullable Map<String, Object> subjectParams) {
        this.subjectKey = subjectKey;
        this.subjectParams = subjectParams == null ? Collections.emptyMap() : subjectParams;
        return this;
    }

    /**
     * Specifies the type of the mail primarily used for logging.
     *
     * @param type the type to use.
     * @return the builder itself
     */
    public MailSender type(String type) {
        this.type = type;
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
     * Renders the given template and uses it as text part.
     *
     * @param template the name of the template to render
     * @param context  the context passed to the renderer
     * @return the builder itself
     */
    public MailSender textTemplate(String template, @Nonnull Context context) {
        this.textTemplate = template;
        this.textContext = context;
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
     * Renders the given template and uses it as HTML part.
     *
     * @param template the name of the template to render
     * @param context  the context passed to the renderer
     * @return the builder itself
     */
    public MailSender htmlTemplate(String template, @Nonnull Context context) {
        this.htmlTemplate = template;
        this.htmlContext = context;
        return this;
    }

    /**
     * Adds an attachment to the email.
     * <p>
     * Use {@link Generator#generateAttachment(String)} to directly generate an attachment from a
     * template.
     *
     * @param attachment the attachment to add to the email
     * @return the builder itself
     */
    public MailSender addAttachment(DataSource attachment) {
        attachments.add(attachment);
        return this;
    }

    /**
     * Adds a resource as attachment.
     * <p>
     * This only adds a static file as attachment. Use {@link #addAttachment(DataSource)} and {@link
     * Generator#generateAttachment(String)} to evaluate a template.
     *
     * @param resource  the resource to lookup using {@link Resources#resolve(String)}
     * @param filename  the filename to use for the attachment
     * @param contentId the content id to reference it within HTML content. &lt; and &gt; are automatically added.
     *                  Note that most mail clients like to see a valid message id (xxx@domain.tld) here.
     * @return the builder itself
     */
    public MailSender addResourceAsAttachment(@Nonnull String resource,
                                              @Nullable String filename,
                                              @Nullable String contentId) {
        Resource res = resources.resolve(resource)
                                .orElseThrow(() -> Exceptions.handle()
                                                             .to(Mails.LOG)
                                                             .withSystemErrorMessage(
                                                                     "Cannot resolve %s as mail attachment",
                                                                     resource)
                                                             .handle());

        String effectiveFilename = Strings.isFilled(filename) ? filename : Files.getFilenameAndExtension(resource);
        ResourceAttachment attachment =
                new ResourceAttachment(effectiveFilename, MimeHelper.guessMimeType(effectiveFilename), res, false);
        if (Strings.isFilled(contentId)) {
            attachment.addHeader("Content-ID", "<" + contentId + ">");
        }
        attachments.add(attachment);

        return this;
    }

    /**
     * Adds a resource as attachment.
     * <p>
     * This can be called from within a template to reference the template directly.
     *
     * @param resource the resource to lookup using {@link Resources#resolve(String)}
     * @param filename the filename to use for the attachment
     * @return the content id to reference it within HTML content. &lt; and &gt; are automatically added.
     * @see #addResourceAsAttachment(String, String, String)
     */
    public String addResourceAsAttachment(@Nonnull String resource, @Nullable String filename) {
        String cid = Strings.generateCode(16) + "@mail.local";
        addResourceAsAttachment(resource, filename, cid);

        return cid;
    }

    /**
     * Adds an array of attachments to the email.
     *
     * @param attachmentsToAdd the attachments to add
     * @return the builder itself
     */
    public MailSender addAttachments(DataSource... attachmentsToAdd) {
        if (attachmentsToAdd != null) {
            attachments.addAll(Arrays.asList(attachmentsToAdd));
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
     * This bounce token is hopefully included in a bounce email (generated if a mail cannot be delivered).
     * This permits better bounce handling.
     *
     * @param token the token to identify the mail by a bounce handler.
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
     * Sets the language used to perform {@link sirius.kernel.nls.NLS} lookups when rendering templates.
     *
     * @param langs an array of languages. The first non-empty value is used.
     * @return the builder itself
     */
    public MailSender setLang(String... langs) {
        if (langs == null) {
            return this;
        }
        for (String language : langs) {
            if (Strings.isFilled(language)) {
                this.lang = language;
                return this;
            }
        }
        return this;
    }

    /**
     * Sends the mail using the given settings.
     * <p>
     * Once all settings are validated, the mail is sent in a separate thread so this method will
     * return rather quickly. Note that a {@link sirius.kernel.health.HandledException} is thrown in case
     * of invalid settings (bad mail address etc.).
     */
    public void send() {
        String tmpLang = NLS.getCurrentLang();
        try {
            try {
                if (lang != null) {
                    CallContext.getCurrent().setLang(lang);
                }
                render();
                buildSubject();
                sanitize();
                check();
                sendMailAsync(smtpConfiguration != null ?
                              smtpConfiguration :
                              UserContext.getCurrentScope()
                                         .tryAs(SMTPConfiguration.class)
                                         .orElse(SMTPConfiguration.fromConfig()));
            } finally {
                CallContext.getCurrent().setLang(tmpLang);
            }
        } catch (HandledException e) {
            throw e;
        } catch (Exception e) {
            throw Exceptions.handle()
                            .withSystemErrorMessage(
                                    "Cannot send mail to '%s (%s)' from '%s (%s)' with subject '%s': %s (%s)",
                                    receiverEmail,
                                    receiverName,
                                    senderEmail,
                                    senderName,
                                    subject)
                            .to(Mails.LOG)
                            .error(e)
                            .handle();
        }
    }

    private void render() {
        if (Strings.isFilled(htmlTemplate)) {
            htmlContent(templates.generator()
                                 .useTemplate(htmlTemplate)
                                 .put("mailContext", this)
                                 .applyContext(htmlContext)
                                 .generate());
        }
        if (Strings.isFilled(textTemplate)) {
            textContent(templates.generator()
                                 .useTemplate(textTemplate)
                                 .put("mailContext", this)
                                 .applyContext(textContext)
                                 .generate());
        }
    }

    private void buildSubject() {
        if (Strings.isFilled(subjectKey)) {
            subject = NLS.fmtr(subjectKey).set(subjectParams).format();
        }
    }

    protected void sendMailAsync(SMTPConfiguration config) {
        if (Strings.isEmpty(config.getMailHost())) {
            Mails.LOG.WARN("Not going to send an email to '%s' with subject '%s' as no mail server is configured...",
                           receiverEmail,
                           subject);
        } else {
            SendMailTask task = new SendMailTask(this, config);
            tasks.executor("email").fork(task);
        }
    }

    private void check() {
        checkReceiver();
        checkSender();
        checkReplyTo();
    }

    private void checkReceiver() {
        try {
            if (Strings.isFilled(receiverName)) {
                new InternetAddress(receiverEmail, receiverName).validate();
            } else {
                new InternetAddress(receiverEmail).validate();
            }
        } catch (Exception e) {
            logInvalidAddress(e, "MailService.invalidReceiver", receiverName, receiverEmail);
        }
    }

    private void logInvalidAddress(Exception e, String nlsKey, String name, String email) {
        throw Exceptions.handle()
                        .to(Mails.LOG)
                        .error(e)
                        .withNLSKey(nlsKey)
                        .set("address", Strings.isFilled(name) ? email + " (" + name + ")" : email)
                        .handle();
    }

    private void checkSender() {
        try {
            if (Strings.isFilled(senderEmail)) {
                if (Strings.isFilled(senderName)) {
                    new InternetAddress(senderEmail, senderName).validate();
                } else {
                    new InternetAddress(senderEmail).validate();
                }
            }
        } catch (Exception e) {
            logInvalidAddress(e, "MailService.invalidSender", senderName, senderEmail);
        }
    }

    private void checkReplyTo() {
        try {
            if (Strings.isFilled(replyToEmail)) {
                if (Strings.isFilled(replyToName)) {
                    new InternetAddress(replyToEmail, replyToName).validate();
                } else {
                    new InternetAddress(replyToEmail).validate();
                }
            }
        } catch (Exception e) {
            logInvalidAddress(e, "MailService.invalidReplyTo", replyToName, replyToEmail);
        }
    }

    private void sanitize() {
        if (Strings.isFilled(senderEmail)) {
            senderEmail = senderEmail.replaceAll("\\s", "");
            if (UserContext.getSettings().get("mail.usePunycode").asBoolean()) {
                senderEmail = IDN.toUnicode(senderEmail);
            }
        }
        if (Strings.isFilled(senderName)) {
            senderName = senderName.trim();
        }
        if (Strings.isFilled(receiverEmail)) {
            receiverEmail = receiverEmail.replaceAll("\\s", "");
            if (UserContext.getSettings().get("mail.usePunycode").asBoolean()) {
                receiverEmail = IDN.toUnicode(receiverEmail);
            }
        }
        if (Strings.isFilled(receiverName)) {
            receiverName = receiverName.trim();
        }
        if (Strings.isFilled(replyToEmail)) {
            replyToEmail = replyToEmail.replaceAll("\\s", "");
        }
        if (Strings.isFilled(replyToName)) {
            replyToName = replyToName.trim();
        }
    }

    /**
     * Returns the language which is set for the mail for example to set NLS-keys in the context to the right language.
     *
     * @return the language
     */
    public String getLang() {
        return lang;
    }
}
