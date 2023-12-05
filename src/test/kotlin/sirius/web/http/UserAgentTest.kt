/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class UserAgentTest {
    @Test
    fun `UserAgent detects mobile Chrome on Android phone`() {
        val r = TestRequest.GET("/test")
        r.request.headers().add(
            "User-Agent",
            "Mozilla/5.0 (Linux; Android 4.0.4; Galaxy Nexus Build/IMM76B) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.133 Mobile Safari/535.19"
        )
        assertTrue(r.getUserAgent().isAndroid())
        assertFalse(r.getUserAgent().isIOS())
        assertTrue(r.getUserAgent().isMobile())
        assertTrue(r.getUserAgent().isPhone())
        assertFalse(r.getUserAgent().isTablet())
        assertFalse(r.getUserAgent().isDesktop())
    }

    @Test
    fun `UserAgent detects mobile Chrome on Android tablet`() {
        val r = TestRequest.GET("/test")
        r.request.headers().add(
            "User-Agent",
            "Mozilla/5.0 (Linux; Android 4.4.2; SM-T230 Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.125 Safari/537.36"
        )
        assertTrue(r.getUserAgent().isAndroid())
        assertFalse(r.getUserAgent().isIOS())
        assertTrue(r.getUserAgent().isMobile())
        assertFalse(r.getUserAgent().isPhone())
        assertTrue(r.getUserAgent().isTablet())
        assertFalse(r.getUserAgent().isDesktop())
    }

    @Test
    fun `UserAgent detects iPhone`() {
        val r = TestRequest.GET("/test")
        r.request.headers().add(
            "User-Agent",
            "Mozilla/5.0 (iPhone; CPU iPhone OS 8_0_2 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12A366 Safari/600.1.4"
        )
        assertFalse(r.getUserAgent().isAndroid())
        assertTrue(r.getUserAgent().isIOS())
        assertTrue(r.getUserAgent().isMobile())
        assertTrue(r.getUserAgent().isPhone())
        assertFalse(r.getUserAgent().isTablet())
        assertFalse(r.getUserAgent().isDesktop())
    }

    @Test
    fun `UserAgent detects iPad`() {
        val r = TestRequest.GET("/test")
        r.request.headers().add(
            "User-Agent",
            "Mozilla/5.0 (iPad; CPU OS 8_1_3 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12B466 Safari/600.1.4"
        )
        assertFalse(r.getUserAgent().isAndroid())
        assertTrue(r.getUserAgent().isIOS())
        assertTrue(r.getUserAgent().isMobile())
        assertFalse(r.getUserAgent().isPhone())
        assertTrue(r.getUserAgent().isTablet())
        assertFalse(r.getUserAgent().isDesktop())
    }

    @Test
    fun `UserAgent detects desktop browser (Firefox)`() {
        val r = TestRequest.GET("/test")
        r.request.headers()
            .add("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:53.0) Gecko/20100101 Firefox/53.0")
        assertFalse(r.getUserAgent().isAndroid())
        assertFalse(r.getUserAgent().isIOS())
        assertFalse(r.getUserAgent().isMobile())
        assertFalse(r.getUserAgent().isPhone())
        assertFalse(r.getUserAgent().isTablet())
        assertTrue(r.getUserAgent().isDesktop())
    }

    @Test
    fun `UserAgent detects desktop browser (Chrome)`() {
        val r = TestRequest.GET("/test")
        r.request.headers().add(
            "User-Agent",
            "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"
        )

        assertFalse(r.getUserAgent().isAndroid())
        assertFalse(r.getUserAgent().isIOS())
        assertFalse(r.getUserAgent().isMobile())
        assertFalse(r.getUserAgent().isPhone())
        assertFalse(r.getUserAgent().isTablet())
        assertTrue(r.getUserAgent().isDesktop())
    }
}