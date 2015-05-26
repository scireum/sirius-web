/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import org.junit.Test;

import java.net.InetAddress;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class IPRangeTest {
    @Test
    public void testSimpleRange() throws Exception {
        assertTrue(IPRange.parseRange("192.168.192.1").matches(InetAddress.getByName("192.168.192.1")));
        assertFalse(IPRange.parseRange("192.168.192.1").matches(InetAddress.getByName("192.168.192.2")));

        assertTrue(IPRange.parseRange("192.168.192.1/32").matches(InetAddress.getByName("192.168.192.1")));
        assertFalse(IPRange.parseRange("192.168.192.1/32").matches(InetAddress.getByName("192.168.192.2")));

        assertTrue(IPRange.parseRange("192.168.192.0/24").matches(InetAddress.getByName("192.168.192.1")));
        assertTrue(IPRange.parseRange("192.168.192.0/24").matches(InetAddress.getByName("192.168.192.2")));
        assertFalse(IPRange.parseRange("192.168.192.0/24").matches(InetAddress.getByName("192.168.191.1")));

        assertTrue(IPRange.parseRange("192.168.0.0/16").matches(InetAddress.getByName("192.168.192.1")));
        assertTrue(IPRange.parseRange("192.168.0.0/16").matches(InetAddress.getByName("192.168.191.2")));
        assertFalse(IPRange.parseRange("192.168.0.0/16").matches(InetAddress.getByName("192.169.192.1")));

        assertTrue(IPRange.parseRange("192.0.0.0/8").matches(InetAddress.getByName("192.168.192.1")));
        assertTrue(IPRange.parseRange("192.0.0.0/8").matches(InetAddress.getByName("192.169.191.1")));
        assertFalse(IPRange.parseRange("192.0.0.0/8").matches(InetAddress.getByName("193.168.192.1")));

        assertTrue(IPRange.parseRange("0.0.0.0/0").matches(InetAddress.getByName("192.168.192.1")));
        assertTrue(IPRange.parseRange("0.0.0.0/0").matches(InetAddress.getByName("193.168.192.1")));
    }

    @Test
    public void testRanges() throws Exception {
        // Test "no filter"
        IPRange.RangeSet set = IPRange.NO_FILTER;
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")));
        assertTrue(set.accepts(InetAddress.getByName("127.0.0.1")));
        assertTrue(set.accepts(InetAddress.getByName("::1")));

        // Test parsing of "no filter"
        set = IPRange.paraseRangeSet("");
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")));
        assertTrue(set.accepts(InetAddress.getByName("127.0.0.1")));
        assertTrue(set.accepts(InetAddress.getByName("::1")));
        set = IPRange.paraseRangeSet(",,");
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")));
        assertTrue(set.accepts(InetAddress.getByName("127.0.0.1")));
        assertTrue(set.accepts(InetAddress.getByName("::1")));

        // Test a simple subnet
        set = IPRange.paraseRangeSet("192.168.0.0/16");
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")));
        assertFalse(set.accepts(InetAddress.getByName("192.167.192.1")));
        // Localhost is no longer added by default
        assertFalse(set.accepts(InetAddress.getByName("127.0.0.1")));
        assertFalse(set.accepts(InetAddress.getByName("::1")));

        // Test a simple subnet
        set = IPRange.paraseRangeSet("192.168.0.0/16, 192.167.12.0/24");
        assertTrue(set.accepts(InetAddress.getByName("192.168.192.1")));
        assertFalse(set.accepts(InetAddress.getByName("192.167.13.1")));
        // Localhost is no longer added by default
        assertFalse(set.accepts(InetAddress.getByName("127.0.0.1")));
        assertFalse(set.accepts(InetAddress.getByName("::1")));
        assertTrue(set.accepts(InetAddress.getByName("192.167.12.1")));
    }

    @Test
    public void testIPv6() throws Exception {
        // Run some simple tests against some IPv6 addresses
        assertTrue(IPRange.parseRange("fe:fe::/32").matches(InetAddress.getByName("fe:fe:05::")));
        assertFalse(IPRange.parseRange("fe:fe::/32").matches(InetAddress.getByName("fe:fa:05::")));
        assertTrue(IPRange.parseRange("fe::/16").matches(InetAddress.getByName("fe:fa:05::")));
    }
}
