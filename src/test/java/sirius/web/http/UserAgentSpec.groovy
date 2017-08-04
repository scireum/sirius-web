/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import sirius.kernel.BaseSpecification

class UserAgentSpec extends BaseSpecification {
    def "UserAgent detects mobile Chrome on Android phone"() {
        when:
        TestRequest r = TestRequest.GET("/test")
        r.request.headers().add("User-Agent", "Mozilla/5.0 (Linux; Android 4.0.4; Galaxy Nexus Build/IMM76B) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.133 Mobile Safari/535.19")
        then:
        r.getUserAgent().isAndroid() == true
        r.getUserAgent().isIOS() == false
        r.getUserAgent().isMobile() == true
        r.getUserAgent().isPhone() == true
        r.getUserAgent().isTablet() == false
        r.getUserAgent().isDesktop() == false
    }

    def "UserAgent detects mobile Chrome on Android tablet"() {
        when:
        TestRequest r = TestRequest.GET("/test")
        r.request.headers().add("User-Agent", "Mozilla/5.0 (Linux; Android 4.4.2; SM-T230 Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.125 Safari/537.36")
        then:
        r.getUserAgent().isAndroid() == true
        r.getUserAgent().isIOS() == false
        r.getUserAgent().isMobile() == true
        r.getUserAgent().isPhone() == false
        r.getUserAgent().isTablet() == true
        r.getUserAgent().isDesktop() == false
    }

    def "UserAgent detects iPhone"() {
        when:
        TestRequest r = TestRequest.GET("/test")
        r.request.headers().add("User-Agent", "Mozilla/5.0 (iPhone; CPU iPhone OS 8_0_2 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12A366 Safari/600.1.4")
        then:
        r.getUserAgent().isAndroid() == false
        r.getUserAgent().isIOS() == true
        r.getUserAgent().isMobile() == true
        r.getUserAgent().isPhone() == true
        r.getUserAgent().isTablet() == false
        r.getUserAgent().isDesktop() == false
    }

    def "UserAgent detects iPad"() {
        when:
        TestRequest r = TestRequest.GET("/test")
        r.request.headers().add("User-Agent", "Mozilla/5.0 (iPad; CPU OS 8_1_3 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12B466 Safari/600.1.4")
        then:
        r.getUserAgent().isAndroid() == false
        r.getUserAgent().isIOS() == true
        r.getUserAgent().isMobile() == true
        r.getUserAgent().isPhone() == false
        r.getUserAgent().isTablet() == true
        r.getUserAgent().isDesktop() == false
    }

    def "UserAgent detects desktop browser (Firefox)"() {
        when:
        TestRequest r = TestRequest.GET("/test")
        r.request.headers().add("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:53.0) Gecko/20100101 Firefox/53.0")
        then:
        r.getUserAgent().isAndroid() == false
        r.getUserAgent().isIOS() == false
        r.getUserAgent().isMobile() == false
        r.getUserAgent().isPhone() == false
        r.getUserAgent().isTablet() == false
        r.getUserAgent().isDesktop() == true
    }

    def "UserAgent detects desktop browser (Chrome)"() {
        when:
        TestRequest r = TestRequest.GET("/test")
        r.request.headers().add("User-Agent", "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
        then:
        r.getUserAgent().isAndroid() == false
        r.getUserAgent().isIOS() == false
        r.getUserAgent().isMobile() == false
        r.getUserAgent().isPhone() == false
        r.getUserAgent().isTablet() == false
        r.getUserAgent().isDesktop() == true
    }
}