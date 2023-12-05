/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */
package sirius.web.http

import org.junit.Test
import java.net.InetAddress
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class IPRangeTest {
    @Test
    @Throws(Exception::class)
    fun testSimpleRange() {
        assertTrue(IPRange.parseRange("192.168.192.1").matches(InetAddress.getByName("192.168.192.1")))
        assertFalse(IPRange.parseRange("192.168.192.1").matches(InetAddress.getByName("192.168.192.2")))
        assertTrue(IPRange.parseRange("192.168.192.1/32").matches(InetAddress.getByName("192.168.192.1")))
        assertFalse(IPRange.parseRange("192.168.192.1/32").matches(InetAddress.getByName("192.168.192.2")))
        assertTrue(IPRange.parseRange("192.168.192.0/24").matches(InetAddress.getByName("192.168.192.1")))
        assertTrue(IPRange.parseRange("192.168.192.0/24").matches(InetAddress.getByName("192.168.192.2")))
        assertFalse(IPRange.parseRange("192.168.192.0/24").matches(InetAddress.getByName("192.168.191.1")))
        assertTrue(IPRange.parseRange("192.168.0.0/16").matches(InetAddress.getByName("192.168.192.1")))
        assertTrue(IPRange.parseRange("192.168.0.0/16").matches(InetAddress.getByName("192.168.191.2")))
        assertFalse(IPRange.parseRange("192.168.0.0/16").matches(InetAddress.getByName("192.169.192.1")))
        assertTrue(IPRange.parseRange("192.0.0.0/8").matches(InetAddress.getByName("192.168.192.1")))
        assertTrue(IPRange.parseRange("192.0.0.0/8").matches(InetAddress.getByName("192.169.191.1")))
        assertFalse(IPRange.parseRange("192.0.0.0/8").matches(InetAddress.getByName("193.168.192.1")))
        assertTrue(IPRange.parseRange("0.0.0.0/0").matches(InetAddress.getByName("192.168.192.1")))
        assertTrue(IPRange.parseRange("0.0.0.0/0").matches(InetAddress.getByName("193.168.192.1")))
    }

    @Test
    @Throws(Exception::class)
    fun testRanges() {
        // Test "no filter"
        var set = IPRange.NO_FILTER
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")))
        assertTrue(set.accepts(InetAddress.getByName("127.0.0.1")))
        assertTrue(set.accepts(InetAddress.getByName("::1")))

        // Test parsing of "no filter"
        set = IPRange.parseRangeSet("")
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")))
        assertTrue(set.accepts(InetAddress.getByName("127.0.0.1")))
        assertTrue(set.accepts(InetAddress.getByName("::1")))
        set = IPRange.parseRangeSet(",,")
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")))
        assertTrue(set.accepts(InetAddress.getByName("127.0.0.1")))
        assertTrue(set.accepts(InetAddress.getByName("::1")))

        // Test a simple subnet
        set = IPRange.parseRangeSet("192.168.0.0/16")
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")))
        assertFalse(set.accepts(InetAddress.getByName("192.167.192.1")))
        // Localhost is no longer added by default
        assertFalse(set.accepts(InetAddress.getByName("127.0.0.1")))
        assertFalse(set.accepts(InetAddress.getByName("::1")))

        // Test a simple subnet
        set = IPRange.parseRangeSet("192.168.0.0/16, 192.167.12.0/24")
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")))
        assertFalse(set.accepts(InetAddress.getByName("192.167.13.1")))
        // Localhost is no longer added by default
        assertFalse(set.accepts(InetAddress.getByName("127.0.0.1")))
        assertFalse(set.accepts(InetAddress.getByName("::1")))
        assertTrue(set.accepts(InetAddress.getByName("192.167.12.1")))
    }

    @Test
    @Throws(Exception::class)
    fun testIPv6() {
        // Run some simple tests against some IPv6 addresses
        assertTrue(IPRange.parseRange("fe:fe::/32").matches(InetAddress.getByName("fe:fe:05::")))
        assertFalse(IPRange.parseRange("fe:fe::/32").matches(InetAddress.getByName("fe:fa:05::")))
        assertTrue(IPRange.parseRange("fe::/16").matches(InetAddress.getByName("fe:fa:05::")))
    }
}
