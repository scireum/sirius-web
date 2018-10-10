/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpHeaderValues
import sirius.kernel.BaseSpecification

class WebContextSpec extends BaseSpecification {

    def "getQueryString returns the full query string"() {
        when:
        TestRequest r = TestRequest.GET("/test?a=a&b=b")
        then:
        r.getParameter("a") == "a"
        and:
        r.getParameter("b") == "b"
        and:
        r.getQueryString() == "a=a&b=b"
    }

    def "getQueryString returns an empty string when no query string is present"() {
        when:
        TestRequest r = TestRequest.GET("/test")
        then:
        r.getQueryString() == ""
    }

    def "getQueryString returns an empty string when an empty query string is present"() {
        when:
        TestRequest r = TestRequest.GET("/test?")
        then:
        r.getQueryString() == ""
    }

    def "withCustomURI rewrites the uri correctly and removes the existing query string"() {
        when:
        TestRequest r = TestRequest.GET("/test?a=a")
        and:
        r.withCustomURI("/test%2Ftest?b=b")
        then:
        r.getRawRequestedURI() == "/test%2Ftest"
        and:
        r.getRequestedURI() == "/test/test"
        and:
        !r.get("a").isFilled()
        and:
        r.get("b").isFilled()
    }

    def "withCustomPath rewrites the path correctly without removing the existing query string"() {
        when:
        TestRequest r = TestRequest.GET("/test?a=a")
        and:
        r.withCustomPath("/test/test")
        then:
        r.getRawRequestedURI() == "/test/test"
        and:
        r.getRequestedURI() == "/test/test"
        and:
        r.get("a").isFilled()
    }


    def "parseAcceptLanguage works as expected"(header, lang) {
        expect:
        TestRequest.GET("/test?a=a").addHeader(HttpHeaderNames.ACCEPT_LANGUAGE, header).getLang() == lang
        where:
        header | lang
        "de, en;q=0.8" | "de"
        "en, de;q=0.8" | "en"
        "xx, de;q=0.8, en-gb;q=0.7" | "de"
        "xx, de;q=0.5, en-gb;q=0.7" | "en"
    }

    def "setSessionValue works as expected"() {
        when:
        HttpURLConnection c = new URL("http://localhost:9999/test/session-test").openConnection()
        c.setRequestMethod("GET")
        c.connect()
        then:
        c.getResponseCode() == 200
        and:
        c.getHeaderFields().get(HttpHeaderNames.SET_COOKIE).get(0).contains("test1=test")
        and:
        !c.getHeaderFields().get(HttpHeaderNames.SET_COOKIE).get(0).contains("test2=")
    }
}
