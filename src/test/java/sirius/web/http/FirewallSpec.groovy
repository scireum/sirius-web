/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import sirius.kernel.BaseSpecification

class FirewallSpec extends BaseSpecification {

    def "unlimited realm isn't blocked"() {
        when:
        HttpURLConnection u = new URL("http://localhost:9999/test/firewall").openConnection()
        and:
        def responseCode = u.getResponseCode()
        then:
        responseCode == 200
    }

    def "blocking IPs works"() {
        when:
        TestFirewall.blockAllIPs = true
        and:
        HttpURLConnection u = new URL("http://localhost:9999/test/firewall").openConnection()
        and:
        def responseCode = u.getResponseCode()
        then:
        thrown(SocketException)
        cleanup:
        TestFirewall.blockAllIPs = false
    }

    def "blocked realm is blocked"() {
        when:
        HttpURLConnection u = new URL("http://localhost:9999/test/firewallBlocked").openConnection()
        and:
        def responseCode = u.getResponseCode()
        then:
        responseCode == 429
    }

}
