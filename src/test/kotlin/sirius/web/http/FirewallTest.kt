/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import java.net.HttpURLConnection
import java.net.SocketException
import java.net.URL
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class FirewallTest {

    @Test
    fun `unlimited realm isn't blocked`() {

        val url = URL("http://localhost:9999/test/firewall").openConnection() as HttpURLConnection

        val responseCode = url.getResponseCode()

        assertEquals(200, responseCode)
    }

    @Test
    fun `blocking IPs works`() {

        TestFirewall.blockAllIPs = true

        val url = URL("http://localhost:9999/test/firewall").openConnection() as HttpURLConnection

        assertThrows<SocketException> { val responseCode = url.getResponseCode() }

        TestFirewall.blockAllIPs = false
    }

    @Test
    fun `blocked realm is blocked`() {

        val url = URL("http://localhost:9999/test/firewallBlocked").openConnection() as HttpURLConnection

        val responseCode = url.getResponseCode()

        assertEquals(429, responseCode)
    }

}
