/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.google.common.base.Charsets
import com.google.common.io.ByteStreams
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpResponseStatus
import sirius.kernel.BaseSpecification

class CSRFTokenSpec extends BaseSpecification{

    def "CSRF security token works correctly if missing via GET"() {
        when:
        def result = TestRequest.GET("/test/fake-delete-data").execute()
        then:
        result.getStatus() == HttpResponseStatus.INTERNAL_SERVER_ERROR
    }

    def "CSRF security token works correctly if missing via POST"() {
        when:
        def result = TestRequest.POST("/test/fake-delete-data").execute()
        then:
        result.getStatus() == HttpResponseStatus.INTERNAL_SERVER_ERROR
    }

    def "CSRF security tokens works correctly if present via POST"() {
        given:
        HttpURLConnection c = new URL("http://localhost:9999/test/provide-security-token").openConnection()
        c.setRequestMethod("GET")
        c.connect()
        def token = new String(ByteStreams.toByteArray(c.getInputStream()), Charsets.UTF_8)

        when:
        HttpURLConnection c2 = new URL("http://localhost:9999/test/fake-delete-data?CSRFToken=" + token).openConnection()
        c2.setRequestMethod("POST")
        c2.setRequestProperty(HttpHeaderNames.COOKIE.toString(), c.getHeaderFields().get("set-cookie").get(0))
        c2.connect()
        then:
        c2.getResponseCode() == 200
    }

    def "CSRF security token works correctly if present via POST but wrong"() {
        given:
        HttpURLConnection c = new URL("http://localhost:9999/test/provide-security-token").openConnection()
        c.setRequestMethod("GET")
        c.connect()
        def token = new String(ByteStreams.toByteArray(c.getInputStream()), Charsets.UTF_8)

        when:
        HttpURLConnection c2 = new URL("http://localhost:9999/test/fake-delete-data?CSRFToken=w-r-o-n-g-t-o-k-e-n").openConnection()
        c2.setRequestMethod("POST")
        c2.setRequestProperty(HttpHeaderNames.COOKIE.toString(), c.getHeaderFields().get("set-cookie").get(0))
        c2.connect()
        then:
        c2.getResponseCode() == 500
    }

    def "CSRF security token works correctly if GET is used"() {
        given:
        HttpURLConnection c = new URL("http://localhost:9999/test/provide-security-token").openConnection()
        c.setRequestMethod("GET")
        c.connect()
        def token = new String(ByteStreams.toByteArray(c.getInputStream()), Charsets.UTF_8)

        when:
        HttpURLConnection c2 = new URL("http://localhost:9999/test/fake-delete-data?CSRFToken=" + token).openConnection()
        c2.setRequestMethod("GET")
        c2.setRequestProperty(HttpHeaderNames.COOKIE.toString(), c.getHeaderFields().get("set-cookie").get(0))
        c2.connect()
        then:
        c2.getResponseCode() == 500
    }
}
