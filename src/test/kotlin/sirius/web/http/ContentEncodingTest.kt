/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.health.HandledException
import sirius.web.controller.Controller
import java.util.*
import kotlin.test.assertEquals

/**
 * Tests the negotiation of request body encodings as specified by RFC 7694.
 */
@ExtendWith(SiriusExtension::class)
class ContentEncodingTest {

    companion object {
        private val GZIP_ONLY = setOf(ContentEncoding.GZIP)
        private const val HTTP_UNSUPPORTED_MEDIA_TYPE = 415
    }

    @Test
    fun `unencoded bodies are always accepted`() {
        assertEquals(Optional.empty(), ContentEncoding.negotiate(listOf(), setOf()))
        assertEquals(Optional.empty(), ContentEncoding.negotiate(listOf("identity"), setOf()))
        assertEquals(Optional.empty(), ContentEncoding.negotiate(listOf("Identity, "), setOf()))
    }

    @Test
    fun `accepted encodings are matched ignoring case and aliases`() {
        assertEquals(Optional.of(ContentEncoding.GZIP), ContentEncoding.negotiate(listOf("gzip"), GZIP_ONLY))
        assertEquals(Optional.of(ContentEncoding.GZIP), ContentEncoding.negotiate(listOf(" GZip "), GZIP_ONLY))
        assertEquals(Optional.of(ContentEncoding.GZIP), ContentEncoding.negotiate(listOf("x-gzip"), GZIP_ONLY))
        assertEquals(
            Optional.of(ContentEncoding.GZIP),
            ContentEncoding.negotiate(listOf("identity", "gzip, identity"), GZIP_ONLY)
        )
    }

    @Test
    fun `encodings not accepted by the route are rejected with identity`() {
        assertRejected(listOf("gzip"), setOf(), "identity")
    }

    @Test
    fun `unknown encodings are rejected with the accepted encodings`() {
        assertRejected(listOf("deflate"), GZIP_ONLY, "gzip")
    }

    @Test
    fun `stacked encodings are rejected`() {
        assertRejected(listOf("gzip, gzip"), GZIP_ONLY, "gzip")
        assertRejected(listOf("gzip", "gzip"), GZIP_ONLY, "gzip")
    }

    private fun assertRejected(headers: List<String>, acceptedEncodings: Set<ContentEncoding>, acceptEncoding: String) {
        val error = assertThrows<HandledException> { ContentEncoding.negotiate(headers, acceptedEncodings) }

        assertEquals(HTTP_UNSUPPORTED_MEDIA_TYPE, error.getHint(Controller.HTTP_STATUS).asInt(0))
        assertEquals(acceptEncoding, error.getHint(Controller.HTTP_HEADER_ACCEPT_ENCODING).asString())
    }
}
