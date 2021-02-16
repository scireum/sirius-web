/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Average;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.health.metrics.MetricProvider;
import sirius.kernel.health.metrics.MetricsCollector;
import sirius.web.resources.Resources;
import sirius.web.templates.Templates;

import javax.annotation.Nullable;
import javax.mail.internet.InternetAddress;
import java.util.Collection;

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

    @Part
    private Resources resources;

    @Part
    private Templates templates;

    @Part
    private Tasks tasks;

    @Parts(MailLog.class)
    private Collection<MailLog> logs;

    private final Average mailsOut = new Average();

    @Override
    public void gather(MetricsCollector collector) {
        collector.metric("mails_out", "mails-out", "Mails Sent", mailsOut.getCount(), null);
        collector.metric("mails_duration", "mails-duration", "Send Mail Duration", mailsOut.getAndClear(), "ms");
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
        } catch (Exception e) {
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
}
