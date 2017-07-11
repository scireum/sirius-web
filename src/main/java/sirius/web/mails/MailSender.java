/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.typesafe.config.Config;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.kernel.settings.Extension;
import sirius.web.http.MimeHelper;
import sirius.web.templates.Resource;
import sirius.web.templates.Resources;
import sirius.web.templates.Templates;
import sirius.web.templates.velocity.VelocityContentHandler;

import javax.activation.DataSource;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.mail.internet.InternetAddress;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Implements the builder pattern to specify the mail to send.
 */
public class MailSender {

    public static final String CONFIG_KEY_HEADERS = "headers";
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
     * Sets the language used to perform {@link sirius.kernel.nls.NLS} lookups when rendering templates.
     *
     * @param langs an array of languages. The first non empty value is used.
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
     * This is boilerpalte for {@code fromEmail(senderEmail).fromName(senderName)}
     *
     * @param senderEmail the email address which sent the email
     * @param senderName  the name of the sender of the email
     * @return the buidler itself
     */
    public MailSender from(String senderEmail, String senderName) {
        return fromEmail(senderEmail).fromName(senderName);
    }

    /**
     * Specifies both, the receiver email and name.
     * <p>
     * This is boilerpalte for {@code toEmail(receiverEmail).toName(receiverName)}
     *
     * @param receiverEmail the email address which should receive the email
     * @param receiverName  the name of the receiver of the email
     * @return the buidler itself
     */
    public MailSender to(String receiverEmail, String receiverName) {
        return toEmail(receiverEmail).toName(receiverName);
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
     * <p>
     * Use {@link Templates.Generator#generateAttachment(String)} to directly generate an attachment from a
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
        String tmpLang = NLS.getCurrentLang();
        try {
            try {
                if (lang != null) {
                    CallContext.getCurrent().setLang(lang);
                }
                fill();
                sanitize();
                check();
                sendMailAsync(new SMTPConfiguration());
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
        try {
            if (Strings.isFilled(receiverName)) {
                new InternetAddress(receiverEmail, receiverName).validate();
            } else {
                new InternetAddress(receiverEmail).validate();
            }
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(Mails.LOG)
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
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(Mails.LOG)
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
            if (ex.getConfig(CONFIG_KEY_HEADERS) != null) {
                for (Map.Entry<String, com.typesafe.config.ConfigValue> e : ex.getConfig(CONFIG_KEY_HEADERS)
                                                                              .entrySet()) {
                    headers.put(e.getKey(), NLS.toMachineString(e.getValue().unwrapped()));
                }
            }
            generateAttachments(ex);
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

    private void generateAttachments(Extension ex) {
        for (Config attachmentConfig : ex.getConfigs("attachments")) {
            String template = attachmentConfig.getString("template");
            try {
                generateAttachment(attachmentConfig, template);
            } catch (Exception t) {
                Exceptions.handle()
                          .to(Mails.LOG)
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

    private void generateAttachment(Config attachmentConfig, String template) throws IOException {
        byte[] contents = generateAttachmentContents(attachmentConfig, template);
        String fileName = determineAttachmentFilename(attachmentConfig);
        String mimeType = determineAttachmentMimeType(attachmentConfig, fileName);
        boolean asAlternative = determineIfAttachmentIsAlternative(attachmentConfig);

        Attachment attachment = new BufferedAttachment(fileName, mimeType, contents, asAlternative);
        applyAttachmentHeaders(attachmentConfig, attachment);
        addAttachment(attachment);
    }

    private void applyAttachmentHeaders(Config attachmentConfig, Attachment att) {
        if (attachmentConfig.hasPath(CONFIG_KEY_HEADERS)) {
            for (Map.Entry<String, com.typesafe.config.ConfigValue> e : attachmentConfig.getConfig(CONFIG_KEY_HEADERS)
                                                                                        .entrySet()) {
                att.addHeader(e.getKey(), NLS.toMachineString(e.getValue().unwrapped()));
            }
        }
    }

    private boolean determineIfAttachmentIsAlternative(Config attachmentConfig) {
        boolean asAlternative = false;
        if (attachmentConfig.hasPath("alternative")) {
            asAlternative = attachmentConfig.getBoolean("alternative");
        }
        return asAlternative;
    }

    private String determineAttachmentMimeType(Config attachmentConfig, String fileName) {
        String mimeType = MimeHelper.guessMimeType(fileName);
        if (attachmentConfig.hasPath("contentType")) {
            mimeType = attachmentConfig.getString("contentType");
        }
        return mimeType;
    }

    private String determineAttachmentFilename(Config attachmentConfig) {
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
        return fileName;
    }

    private byte[] generateAttachmentContents(Config attachmentConfig, String template) throws IOException {
        Templates.Generator attachment = templates.generator();
        if (attachmentConfig.hasPath("encoding")) {
            attachment.encoding(attachmentConfig.getString("encoding"));
        }
        attachment.useTemplate(template);
        attachment.applyContext(context);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        attachment.generateTo(out);
        out.flush();
        return out.toByteArray();
    }

    private void fillTextContent(Extension ex) {
        textContent(templates.generator()
                             .useTemplate(ex.get("text_" + NLS.getCurrentLang()).asString(ex.get("text").asString()))
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
        } catch (Exception e) {
            Exceptions.handle()
                      .to(Mails.LOG)
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
        Extension ex = Sirius.getSettings().getExtension("mail.templates", mailExtension);
        if (ex == null) {
            throw Exceptions.handle()
                            .withSystemErrorMessage("Unknown mail extension: %s. Cannot send mail from: '%s' to '%s'",
                                                    mailExtension,
                                                    senderEmail,
                                                    receiverEmail)
                            .handle();
        }
        return ex;
    }
}
