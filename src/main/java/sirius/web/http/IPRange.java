/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.google.common.collect.Lists;
import sirius.kernel.commons.Strings;

import java.math.BigInteger;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;

/**
 * Represents a range of IP addresses in CIDR notation.
 * <p>
 * Valid inputs are x.x.x.x/bits or z:z:z:z:z:z:z:z/bits. If /bits is missing, /32 vor IPv4 or /128 for IPv6 is
 * assumed.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2013/11
 */
public class IPRange {

    /*
     * Internal bitmask representing the IP range. We use BigInteger internally since primitive types are
     * signed and computations get nasty.
     */
    private BigInteger baseIP = BigInteger.ZERO;

    /*
     * Represents the full mask, meaning that all bits of the given baseIP need to be applied
     */
    private static BigInteger COMPLETE_MASK = new BigInteger("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16);

    /*
     * Contains the effective mask. This determines which bits of baseIP are used to check against an incoming IP.
     */
    private BigInteger mask = COMPLETE_MASK;

    /*
     * Contains a string representation of this range
     */
    private String stringRepresentation = "*";

    /**
     * Parses a CIDR expression and returns an <tt>IPRange</tt>
     *
     * @param cidr the CIDR expression to parse. This can be either an IP address like 192.168.1.4 or a subnet like
     *             192.168.192.0/24 or empty which implies 0.0.0.0/0
     * @return an IPRange representing the given cidr expression.
     */
    public static IPRange parseRange(String cidr) throws IllegalArgumentException {
        IPRange result = new IPRange();
        try {
            if (Strings.isFilled(cidr)) {
                result.stringRepresentation = cidr;
                String[] input = cidr.split("/");
                boolean IPv6 = cidr.contains(":");
                result.baseIP = ipToInt(InetAddress.getByName(input[0]));
                if (input.length > 1) {
                    result.mask = COMPLETE_MASK.shiftLeft((IPv6 ? 128 : 32) - Integer.parseInt(input[1]));
                }
            }
        } catch (Throwable e) {
            throw new IllegalArgumentException(Strings.apply("Invalid IP range given: %s - %s", cidr, e.getMessage()));
        }
        return result;
    }

    /*
     * Converts an IP address to a 32 bit integer - using BigInteger because Java has no unsigned types.
     */
    private static BigInteger ipToInt(InetAddress address) {
        return new BigInteger(address.getAddress());
    }

    /**
     * Checks if the given IP is within the described ip range.
     *
     * @param address the address to check
     * @return <tt>true</tt> if the given address is in the described IP range, <tt>false</tt> otherwise
     */
    public boolean matches(InetAddress address) {
        if (BigInteger.ZERO.equals(baseIP)) {
            return true;
        }
        BigInteger ip = ipToInt(address);
        if (BigInteger.ZERO.equals(ip)) {
            return true;
        }
        return baseIP.and(mask).and(COMPLETE_MASK).equals(ip.and(mask).and(COMPLETE_MASK));
    }

    /**
     * Constant for a "non filtering" range set which accepts all IP addresses
     */
    public static final RangeSet NO_FILTER = new RangeSet();
    /**
     * Constant for a range which only accepts localhost.
     */
    public static final RangeSet LOCALHOST = createLocalHostRangeSet();

    /**
     * Represents the IP address of the localhost in an IPv4 environment
     */
    public static final IPRange IPV4_LOCALHOST = IPRange.parseRange("127.0.0.1");

    /**
     * Represents the IP address of the localhost in an IPv6 environment
     */
    public static final IPRange IPV6_LOCALHOST = IPRange.parseRange("0:0:0:0:0:0:0:1");

    /*
     * Computes a RangeSet which only accepts localhost
     */
    private static RangeSet createLocalHostRangeSet() {
        RangeSet result = new RangeSet();
        result.ranges = Lists.newArrayList();
        // Do not use the constants defined above, as these might not be initialized yet.
        // As the definition of localhost shouldn't change, we live with double defined constants rather than
        // using static/lazy initializer magic...
        result.ranges.add(IPRange.parseRange("127.0.0.1"));
        result.ranges.add(IPRange.parseRange("0:0:0:0:0:0:0:1"));
        return result;
    }

    /**
     * Compiles the given list of ranges into a range set.
     *
     * @param commaSeparatedListOfRanges a string defining a list of ip ranges separated by a ",". Each ip range
     *                                   can be an address or a sub net in CIDR notation.
     * @return a RangeSet representing the given input. If the input was empty, a "no filter" set is returned, which
     *         accepts all IPs.
     */
    public static RangeSet paraseRangeSet(String commaSeparatedListOfRanges) {
        RangeSet result = new RangeSet();
        if (Strings.isFilled(commaSeparatedListOfRanges)) {
            result.ranges = Lists.newArrayList();
            for (String range : commaSeparatedListOfRanges.replace(" ", "").split("[,;]")) {
                if (Strings.isFilled(range)) {
                    result.ranges.add(IPRange.parseRange(range.trim()));
                }
            }
            if (result.ranges.isEmpty()) {
                // No rules were parsed -> accept everything with a fast ==
                result.ranges = null;
            } else {
                // We always accept "localhost"
                result.ranges.add(IPV4_LOCALHOST);
                result.ranges.add(IPV6_LOCALHOST);
            }
        }
        return result;
    }

    @Override
    public String toString() {
        return stringRepresentation;
    }

    /**
     * Represents a set of IP ranges.
     * <p>
     * Ranges can be specified as a list of CIDR sub nets, separated by a ",".
     *
     * @author Andreas Haufler (aha@scireum.de)
     * @since 2013/11
     */
    public static class RangeSet {

        /*
         * List of accepted ranges. If this is <tt>null</tt>, everything is accepted
         */
        private List<IPRange> ranges;

        /*
         * Use compile to create a set of ranges.
         */
        private RangeSet() {

        }

        /**
         * Checks if the given address if accepted by one of the declared ranges, or if
         * the range is completely empty.
         *
         * @param addr the address to check
         * @return <tt>true</tt> if the given address is accepted, <tt>false</tt> otherwise
         */
        public boolean accepts(InetAddress addr) {
            if (addr == null || ranges == null) {
                return true;
            }
            for (IPRange range : ranges) {
                if (range.matches(addr)) {
                    return true;
                }
            }

            return false;
        }

        /**
         * Determines if a filter range is given or not
         *
         * @return <tt>true</tt> if no filter range is given. This implies "no filtering" and will accept all IPs. If
         *         at least one range is given, <tt>false</tt> will be returned
         */
        public boolean isEmpty() {
            return ranges == null;
        }

        @Override
        public String toString() {
            return Strings.join(ranges, ", ");
        }
    }

}
