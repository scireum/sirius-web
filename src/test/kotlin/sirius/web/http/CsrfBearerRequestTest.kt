/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.typesafe.config.ConfigFactory
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpResponseStatus
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.Sirius
import sirius.kernel.SiriusExtension
import sirius.web.controller.ControllerDispatcher
import sirius.web.security.ScopeInfo
import sirius.web.security.UserContext
import kotlin.test.assertEquals

/**
 * Verifies that the CSRF token validation exempts requests which present a bearer token without carrying the
 * session cookie - but only in scopes which enable the exemption via
 * [ControllerDispatcher.CONFIG_KEY_EXEMPT_BEARER_REQUESTS].
 *
 * The exemption is disabled by default, so the tests enabling it install a scope which overrides the setting.
 */
@ExtendWith(SiriusExtension::class)
class CsrfBearerRequestTest {

    private fun sessionCookieName(): String = Sirius.getSettings().getString("http.sessionCookie.name")

    private fun installScopeExemptingBearerRequests() {
        val scope = ScopeInfo("csrf-bearer-test", "default", "csrf-bearer-test", null, {
            ConfigFactory.parseString("${ControllerDispatcher.CONFIG_KEY_EXEMPT_BEARER_REQUESTS} = true")
        }, null)
        UserContext.get().setCurrentScope(scope)
    }

    private fun bearerPost(authorization: String): TestRequest {
        installScopeExemptingBearerRequests()
        return TestRequest.POST("/test/fake-delete-data").addHeader(HttpHeaderNames.AUTHORIZATION, authorization)
    }

    @Test
    fun `A bearer request without session cookie needs no CSRF token if the scope enables the exemption`() {
        val result = bearerPost("Bearer some-token").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `The bearer scheme is matched case-insensitively`() {
        val result = bearerPost("bearer some-token").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `A bearer request without session cookie needs a CSRF token by default`() {
        val result = TestRequest.POST("/test/fake-delete-data")
            .addHeader(HttpHeaderNames.AUTHORIZATION, "Bearer some-token")
            .execute()

        assertEquals(HttpResponseStatus.FORBIDDEN, result.status)
    }

    @Test
    fun `A bearer header without a credential needs a CSRF token`() {
        assertEquals(HttpResponseStatus.FORBIDDEN, bearerPost("Bearer").execute().status)
        assertEquals(HttpResponseStatus.FORBIDDEN, bearerPost("Bearer   ").execute().status)
    }

    @Test
    fun `A bearer request carrying the session cookie needs a CSRF token`() {
        val result = bearerPost("Bearer some-token").addCookie(sessionCookieName(), "any-value").execute()

        assertEquals(HttpResponseStatus.FORBIDDEN, result.status)
    }

    @Test
    fun `A bearer request carrying the session cookie passes with a CSRF token`() {
        installScopeExemptingBearerRequests()
        val result = TestRequest.SAFEPOST("/test/fake-delete-data")
            .addCookie(sessionCookieName(), "any-value")
            .addHeader(HttpHeaderNames.AUTHORIZATION, "Bearer some-token")
            .execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `A request with another authorization scheme needs a CSRF token`() {
        // Browsers attach basic credentials automatically, so they are ambient just like a cookie.
        val result = bearerPost("Basic dXNlcjpwYXNzd29yZA==").execute()

        assertEquals(HttpResponseStatus.FORBIDDEN, result.status)
    }

    @Test
    fun `A request with a server session needs no CSRF token even if it carries the session cookie`() {
        val result = bearerPost("Bearer some-token")
            .addCookie(sessionCookieName(), "any-value")
            .addHeader(TestServerSessionStorage.SESSION_HEADER, "csrf-bearer-session")
            .execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `A failed CSRF validation reports a stable error code`() {
        val result = TestRequest.POST("/test/json").execute()

        assertEquals(HttpResponseStatus.FORBIDDEN, result.status)
        assertEquals(ControllerDispatcher.ERROR_CODE_INVALID_CSRF_TOKEN, result.contentAsJson.get("code").asString())
    }
}
