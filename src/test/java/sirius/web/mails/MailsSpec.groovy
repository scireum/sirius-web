/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails

import sirius.kernel.BaseSpecification
import sirius.kernel.di.std.Part
import sirius.kernel.health.HandledException

class MailsSpec extends BaseSpecification {

    @Part
    static Mails mails

    def "Mails sends a simple mail"() {
        when:
        ((MailsMock) mails).getSentMails().clear()
        mails.createEmail().to("test@scireum.de", "Test").subject("Test eMail").textContent("This is a Test.").send()
        then:
        MailsMock.MailSenderMock mail = ((MailsMock) mails).getLastMail()
        mail.getReceiverEmail() == "test@scireum.de"
        mail.getReceiverName() == "Test"
        mail.getSubject() == "Test eMail"
        mail.getText() == "This is a Test."
        mail.getHtml() == null
    }

    def "Mails rejects an invalid receiver"() {
        when:
        ((MailsMock) mails).getSentMails().clear()
        mails.createEmail().to("test@", "Invalid").subject("Test eMail").textContent("This is a Test.").send()
        then:
        thrown(HandledException)
    }

}
