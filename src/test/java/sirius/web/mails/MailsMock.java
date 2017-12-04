/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import sirius.kernel.commons.Context;
import sirius.kernel.di.Replace;
import sirius.kernel.di.std.Register;

import javax.activation.DataSource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Replace(Mails.class)
@Register(classes = Mails.class)
public class MailsMock extends Mails {

    private ThreadLocal<List<MailSenderMock>> sentMails = new ThreadLocal<>();

    @Override
    public MailSender createEmail() {
        return new MailSenderMock();
    }

    public class MailSenderMock extends MailSender {

        private SMTPConfiguration effectiveConfig;

        @Override
        protected void sendMailAsync(SMTPConfiguration config) {
            this.effectiveConfig = config;
            getSentMails().add(this);
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

        public Context getContext() {
            return context;
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
        List<MailSenderMock> list = sentMails.get();
        if (list == null) {
            list = new ArrayList<>();
            sentMails.set(list);
        }
        return list;
    }

    public MailSenderMock getLastMail() {
        List<MailSenderMock> list = getSentMails();
        return list.isEmpty() ? null : list.get(list.size() - 1);
    }
}
