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
import sirius.web.templates.Generator;
import sirius.web.templates.Templates;

import javax.activation.DataSource;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.mail.internet.InternetAddress;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Implements the builder pattern to specify the mail to send.
 */
public class MailSender {

    protected static final String CONFIG_KEY_HEADERS = "headers";
    protected boolean simulate;
    protected String senderEmail;
    protected String senderName;
    protected String receiverEmail;
    protected String receiverName;
    protected String subject;
    protected Context context;
    protected String text;
    protected String html;
    protected List<DataSource> attachments = Lists.newArrayList();
    protected String bounceToken;
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
    public MailSender textTemplate(String template, Context context) {
        return textContent(templates.generator().useTemplate(template).applyContext(context).generate());
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
    public MailSender htmlTemplate(String template, Context context) {
        return htmlContent(templates.generator().useTemplate(template).applyContext(context).generate());
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
    }
}
