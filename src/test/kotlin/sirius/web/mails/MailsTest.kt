/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Context
import sirius.kernel.di.std.Part
import sirius.kernel.health.HandledException
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class MailsTest {

    companion object {
        @Part
        @JvmStatic
        private lateinit var mails: MailsMock
    }

    @Test
    fun `Mails sends a simple mail`() {

        mails.sentMails.clear()
        mails.createEmail().to("test@scireum.de", "Test").subject("Test eMail").textContent("This is a Test.").send()
        println(mails.lastMail.receiverName)

        val mail = mails.lastMail
        assertEquals("test@scireum.de", mail.getReceiverEmail())
        assertEquals("Test", mail.getReceiverName())
        assertEquals("Test eMail", mail.getSubject())
        assertEquals("This is a Test.", mail.getText())
        assertEquals(null, mail.getHtml())
    }

    @Test
    fun `Mails rejects an invalid receiver`() {
        mails.sentMails.clear()

        assertThrows<HandledException> {
            mails.createEmail().to("test@", "Invalid").subject("Test eMail").textContent("This is a Test.").send()
        }
    }

    @Test
    fun `Mails translates subject to correct language`() {

        mails.sentMails.clear()
        mails.createEmail()
            .to("test@scireum.de", "Test")
            .nlsSubject("mail.subject", Context.create().set("nr", "1"))
            .setLanguage("fr")
            .textContent("This is a Test.")
            .send()

        val mail = mails.lastMail
        assertEquals("Ceci est le test 1.", mail.getSubject())
    }

}
