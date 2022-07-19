/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import jakarta.activation.DataSource;
import sirius.kernel.di.Replace;
import sirius.kernel.di.std.Register;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@Replace(Mails.class)
@Register(classes = Mails.class)
public class MailsMock extends Mails {

    private final List<MailSenderMock> sentMails = Collections.synchronizedList(new ArrayList<>());

    @Override
    public MailSender createEmail() {
        return new MailSenderMock();
    }

    public class MailSenderMock extends MailSender {

        private SMTPConfiguration effectiveConfig;

        @Override
        protected void sendMailAsync() {
            this.effectiveConfig = smtpConfiguration;
            sentMails.add(this);
            Mails.LOG.INFO("eMail to '%s' was not sent but captured, as MailsMock is active...", getReceiverEmail());
        }

        public SMTPConfiguration getEffectiveConfig() {
            return effectiveConfig;
        }

        public String getSenderEmail() {
            return senderEmail;
        }

        public String getSenderName() {
            return senderName;
        }

        public String getReceiverEmail() {
            return receiverEmail;
        }

        public String getReceiverName() {
            return receiverName;
        }

        public String getSubject() {
            return subject;
        }

        public String getText() {
            return text;
        }

        public String getHtml() {
            return html;
        }

        public List<DataSource> getAttachments() {
            return attachments;
        }

        public String getBounceToken() {
            return bounceToken;
        }

        public Map<String, String> getHeaders() {
            return headers;
        }
    }

    public List<MailSenderMock> getSentMails() {
        return sentMails;
    }

    public MailSenderMock getLastMail() {
        return sentMails.get(sentMails.size() - 1);
    }
}
