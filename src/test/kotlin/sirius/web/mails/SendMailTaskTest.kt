/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.mails

import jakarta.mail.Message
import jakarta.mail.Session
import jakarta.mail.internet.InternetAddress
import jakarta.mail.internet.MimeMessage
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.api.io.TempDir
import org.simplejavamail.utils.mail.dkim.DkimMessage
import org.simplejavamail.utils.mail.dkim.DomainKeyUtil
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import java.io.ByteArrayOutputStream
import java.io.File
import java.net.DatagramPacket
import java.net.DatagramSocket
import java.net.InetAddress
import java.security.KeyPairGenerator
import java.util.Base64
import java.util.Properties
import kotlin.concurrent.thread
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertIs
import kotlin.test.assertSame

/**
 * Pins the DKIM signature which [SendMailTask] adds to outgoing mails.
 *
 * The signature is created by <tt>utils-mail-dkim</tt>, which computes RSA signatures using the default JDK
 * providers, but still needs Bouncy Castle on the classpath to load. The tests check that the library accepts our
 * key file and settings and that these show up in the signature. Whether the signature is cryptographically valid
 * is left to the tests of the library.
 *
 * Before signing, the library fetches the public key from the DNS and checks that it matches the private key. The
 * tests therefore publish the key via a minimal local DNS server.
 */
@ExtendWith(SiriusExtension::class)
class SendMailTaskTest {

    @Test
    fun `signs mails of a DKIM domain as configured`() {
        val signedMessage = sign(createMessage(), SENDER)
        assertIs<DkimMessage>(signedMessage, "The mail was not signed, see the log for the reason")

        val rawMessage = serialize(signedMessage)
        val rawHeaders = rawMessage.substringBefore("\r\n\r\n").split(Regex("\r\n(?![ \t])"))
        val body = rawMessage.substringAfter("\r\n\r\n")
        val tags = parseTags(rawHeaders.single { it.startsWith("DKIM-Signature:", ignoreCase = true) })

        assertEquals("1", tags["v"])
        assertEquals("rsa-sha256", tags["a"])
        assertEquals("simple/relaxed", tags["c"])
        assertEquals(DOMAIN, tags["d"])
        assertEquals(SELECTOR, tags["s"])
        assertEquals(SENDER, tags["i"])
        assertEquals(body.length.toString(), tags["l"])
        assertFalse(tags.containsKey("z"), "The signed headers must not be copied into the signature")
        assertFalse(tags["bh"].isNullOrEmpty(), "The signature must contain a body hash")
        assertFalse(tags["b"].isNullOrEmpty(), "The signature must contain the signature data")
    }

    @Test
    fun `leaves mails of other domains unsigned`() {
        val message = createMessage()

        assertSame(message, sign(message, "sender@other.example"))
    }

    @BeforeEach
    fun enableDkim(@TempDir tempDir: File) {
        // 1024 bits keep the public key within a single character string (255 bytes) of the TXT record.
        val keyPair = KeyPairGenerator.getInstance("RSA").apply { initialize(1024) }.generateKeyPair()
        val keyFile = File(tempDir, "dkim.der")
        // SendMailTask expects the private key as unencrypted PKCS#8 in DER, which is the JDK's native encoding.
        keyFile.writeBytes(keyPair.private.encoded)

        DKIM_SETTINGS.forEach { originalSettings[it] = readStaticField(it) }
        writeStaticField("dkimKeyFile", keyFile.absolutePath)
        writeStaticField("dkimDomains", listOf(DOMAIN))
        writeStaticField("dkimSelector", SELECTOR)
        resetDkimCaches()

        val socket = DatagramSocket(0, InetAddress.getByName(DNS_HOST))
        dnsSocket = socket
        serveTxtRecord(socket, "v=DKIM1; k=rsa; p=" + Base64.getEncoder().encodeToString(keyPair.public.encoded))
        originalDnsProviderUrl = DomainKeyUtil.getDnsProviderUrl()
        DomainKeyUtil.setDnsProviderUrl("dns://$DNS_HOST:${socket.localPort}")
    }

    @AfterEach
    fun restoreDkimSettings() {
        originalSettings.forEach { (name, value) -> writeStaticField(name, value) }
        resetDkimCaches()

        // The socket is missing if the setup failed early; the settings above still had to be restored in that case.
        dnsSocket?.let {
            DomainKeyUtil.setDnsProviderUrl(originalDnsProviderUrl)
            it.close()
        }
    }

    private fun createMessage(): MimeMessage {
        val message = MimeMessage(Session.getInstance(Properties()))
        message.setFrom(InternetAddress(SENDER))
        message.setRecipients(Message.RecipientType.TO, "receiver@scireum.example")
        message.subject = "Test eMail"
        message.setText("This is a Test.\r\n")
        return message
    }

    /**
     * Runs the given message through the (private) signing step of [SendMailTask], as a mail of the given sender.
     */
    private fun sign(message: MimeMessage, sender: String): MimeMessage {
        val task = SendMailTask(mails.createEmail().fromEmail(sender), SMTPConfiguration.create())
        val signMessage = SendMailTask::class.java.getDeclaredMethod("signMessage", MimeMessage::class.java)
        signMessage.isAccessible = true
        return signMessage.invoke(task, message) as MimeMessage
    }

    private fun serialize(message: MimeMessage): String {
        val buffer = ByteArrayOutputStream()
        message.writeTo(buffer)
        return buffer.toString(Charsets.ISO_8859_1)
    }

    /**
     * Parses the <tt>tag=value</tt> list of a DKIM signature header, dropping all folding whitespace.
     */
    private fun parseTags(rawSignatureHeader: String): Map<String, String> {
        return rawSignatureHeader.substringAfter(":").split(";").filter { it.isNotBlank() }.associate {
            it.substringBefore("=").trim() to it.substringAfter("=").replace(Regex("\\s"), "")
        }
    }

    /**
     * Answers every DNS query received on the given socket with the given TXT record, until the socket is closed or
     * a query cannot be answered.
     */
    private fun serveTxtRecord(socket: DatagramSocket, text: String) {
        thread(isDaemon = true, name = "dkim-test-dns") {
            val buffer = ByteArray(512)
            try {
                while (true) {
                    val query = DatagramPacket(buffer, buffer.size)
                    socket.receive(query)
                    val response = createTxtResponse(query.data.copyOf(query.length), text)
                    socket.send(DatagramPacket(response, response.size, query.socketAddress))
                }
            } catch (_: Exception) {
                // The socket has been closed or the query was malformed. The latter lets the lookup of the test fail.
            }
        }
    }

    /**
     * Builds an authoritative DNS response (RFC 1035, section 4) which repeats the question of the given query and
     * answers it with a single TXT record.
     */
    private fun createTxtResponse(query: ByteArray, text: String): ByteArray {
        val response = ByteArrayOutputStream()
        val endOfQuestion = query.indexOf(0, HEADER_LENGTH) + 5

        // Header: the ID of the query, flags (response, authoritative), one question, one answer.
        response.write(query, 0, 2)
        response.write(byteArrayOf(0x84.toByte(), 0, 0, 1, 0, 1, 0, 0, 0, 0))
        response.write(query, HEADER_LENGTH, endOfQuestion - HEADER_LENGTH)

        // Answer: a pointer to the name in the question, type TXT, class IN, a TTL of 60s and the length-prefixed text.
        response.write(byteArrayOf(0xC0.toByte(), HEADER_LENGTH.toByte(), 0, 16, 0, 1, 0, 0, 0, 60))
        val data = text.toByteArray(Charsets.US_ASCII)
        require(data.size <= 255) { "A TXT record with several character strings is not supported" }
        response.write(byteArrayOf(((data.size + 1) shr 8).toByte(), (data.size + 1).toByte(), data.size.toByte()))
        response.write(data)
        return response.toByteArray()
    }

    /**
     * Finds the first occurrence of the given value at or after the given index, failing if there is none.
     */
    private fun ByteArray.indexOf(value: Byte, startIndex: Int): Int {
        return (startIndex until size).first { this[it] == value }
    }

    /**
     * Makes [SendMailTask] re-evaluate the DKIM settings, as it caches whether DKIM is enabled and for which domains.
     */
    private fun resetDkimCaches() {
        writeStaticField("dkimEnabled", null)
        writeStaticField("dkimDomainSet", null)
    }

    private fun readStaticField(name: String): Any? {
        return SendMailTask::class.java.getDeclaredField(name).apply { isAccessible = true }.get(null)
    }

    private fun writeStaticField(name: String, value: Any?) {
        SendMailTask::class.java.getDeclaredField(name).apply { isAccessible = true }.set(null, value)
    }

    private var dnsSocket: DatagramSocket? = null
    private var originalDnsProviderUrl: String? = null
    private val originalSettings = mutableMapOf<String, Any?>()

    companion object {
        private const val DOMAIN = "scireum.example"
        private const val SELECTOR = "test"
        private const val DNS_HOST = "127.0.0.1"
        private const val HEADER_LENGTH = 12
        private const val SENDER = "sender@$DOMAIN"
        private val DKIM_SETTINGS = listOf("dkimKeyFile", "dkimDomains", "dkimSelector")

        @Part
        @JvmStatic
        private lateinit var mails: Mails
    }
}
