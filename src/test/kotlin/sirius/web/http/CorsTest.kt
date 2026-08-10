/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.typesafe.config.ConfigFactory
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpMethod
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertAll
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.web.cors.AllowedOrigin
import sirius.web.cors.CorsAllowOriginResolver
import sirius.web.cors.CorsContext
import sirius.web.security.ScopeInfo
import sirius.web.security.UserContext
import sirius.web.security.UserInfo
import java.net.HttpURLConnection
import java.net.URI
import kotlin.test.*

/**
 * Tests the CORS handling of the web server.
 *
 * `http.enableCors` acts as a master switch: only when it is enabled for the request's scope are the interceptors
 * consulted to determine the allowed origin (falling back to reflecting the request's `Origin`). When it is disabled,
 * no CORS headers are emitted at all - not even if an interceptor would allow the origin.
 *
 * Note: Some tests set the restricted `Origin:` header, requiring `-Dsun.net.http.allowRestrictedHeaders=true`. This is
 * enabled centrally in [SiriusExtension.beforeAll]. Requests carrying [TestCorsScopeDetector.HEADER_DISABLE_CORS_ALL]
 * are bound to a scope which disables `enableCors`.
 */
@ExtendWith(SiriusExtension::class)
class CorsTest {

    private val specificAllowedOrigins = setOf("https://a.example.com", "https://b.example.com")

    @AfterEach
    fun resetInterceptorStrategy() {
        TestCorsInterceptor.allowedOrigin = null
    }

    // --- An origin may only be resolved once per request ---

    @Test
    fun `given an origin has already been resolved when tryResolveAndStoreOrigin is called again then the first strategy wins`() {
        val webContext = TestRequest.GET("/system/ok")
        webContext.addHeader(HttpHeaderNames.ORIGIN, "https://first.example.com")

        corsAllowOriginResolver.tryResolveAndStoreOrigin(webContext, AllowedOrigin.MatchRequest())
        corsAllowOriginResolver.tryResolveAndStoreOrigin(webContext, AllowedOrigin.Wildcard())

        assertAll(
            { assertEquals(AllowedOrigin.MatchRequest(), corsAllowOriginResolver.getConfiguredOrigin().orElse(null)) },
            { assertEquals("https://first.example.com", CorsContext.get().getResolvedOrigin().orElse(null)) },
        )
    }

    @Test
    fun `given the first strategy resolves to no origin when tryResolveAndStoreOrigin is called again then the second strategy is still ignored`() {
        val webContext = TestRequest.GET("/system/ok")
        webContext.addHeader(HttpHeaderNames.ORIGIN, "https://evil.example.com")

        corsAllowOriginResolver.tryResolveAndStoreOrigin(webContext, AllowedOrigin.Specific(false, specificAllowedOrigins))
        corsAllowOriginResolver.tryResolveAndStoreOrigin(webContext, AllowedOrigin.Wildcard())

        assertAll(
            {
                assertEquals(
                    AllowedOrigin.Specific(false, specificAllowedOrigins),
                    corsAllowOriginResolver.getConfiguredOrigin().orElse(null)
                )
            },
            { assertNull(CorsContext.get().getResolvedOrigin().orElse(null)) },
        )
    }

    @Test
    fun `given an origin has already been resolved when setConfiguredOrigin or setResolvedOrigin is called directly then an exception is thrown`() {
        val corsContext = CorsContext.get()
        corsContext.setConfiguredOrigin(AllowedOrigin.MatchRequest())
        corsContext.markFinalized()

        assertAll(
            { assertFailsWith<IllegalStateException> { corsContext.setConfiguredOrigin(AllowedOrigin.Wildcard()) } },
            { assertFailsWith<IllegalStateException> { corsContext.setResolvedOrigin("https://evil.example.com") } },
        )
    }

    // --- enableCors master switch ---

    @Test
    fun `given enableCors is enabled when a request carries an origin then it is reflected as 'Access-Control-Allow-Origin'`() {
        assertEquals("TEST", requestAllowedOrigin("TEST"))
    }

    @Test
    fun `given enableCors is disabled then no CORS headers are emitted even if an interceptor would allow the origin`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Wildcard()

        val connection = sendGet("https://example.com", disableCorsAll = true)
        val vary = connection.getHeaderField(HttpHeaderNames.VARY.toString())

        assertAll(
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())) },
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS.toString())) },
            { assertFalse(vary != null && vary.contains(HttpHeaderNames.ORIGIN.toString(), ignoreCase = true)) },
        )
    }

    @Test
    fun `given enableCors is enabled when a preflight request is received then origin, methods and headers are answered`() {
        val connection = sendPreflight(origin = "TEST", requestHeaders = "X-Test")

        assertAll(
            {
                assertEquals(
                    "TEST",
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
                )
            },
            {
                assertContains(
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS.toString()),
                    "GET"
                )
            },
            {
                assertContains(
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_HEADERS.toString()),
                    "X-Test"
                )
            },
        )
    }

    @Test
    fun `given enableCors is enabled when a preflight request is received then 'Access-Control-Allow-Methods' is derived from the registered routes`() {
        // '/test/another-restricted-method' only declares GET, so the preflight must advertise exactly GET and the
        // centrally handled OPTIONS - and not the previously hard-coded "GET,PUT,POST,DELETE".
        val connection = sendPreflight(uri = "/test/another-restricted-method", origin = "TEST")

        val allowedMethods = connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS.toString())
        assertEquals("GET, OPTIONS", allowedMethods)
    }

    @Test
    fun `given a scope overriding enableCors to false when the global setting is enabled then automatic cors is disabled`() {
        UserContext.get().setCurrentScope(configuredScope("corsDisabled", false))
        assertFalse(TestRequest.GET("/system/ok").isCorsEnabled)
    }

    @Test
    fun `given a scope without an enableCors override when the global setting is enabled then the global setting is used`() {
        UserContext.get().setCurrentScope(configuredScope("default", null))
        assertTrue(TestRequest.GET("/system/ok").isCorsEnabled)
    }

    // --- Allowed origin strategies (CORS enabled) ---

    @Test
    fun `given enableCors is enabled when no interceptor decides on a strategy then the requested origin is reflected as fallback`() {
        TestCorsInterceptor.allowedOrigin = null

        assertEquals("https://example.com", requestAllowedOrigin("https://example.com"))
    }

    @ParameterizedTest
    @ValueSource(strings = ["https://example.com", "http://localhost:3000"])
    fun `given enableCors is enabled when the interceptor returns a Wildcard strategy then the asterisk is returned regardless of the requested origin`(
        requestOrigin: String
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Wildcard()

        assertEquals("*", requestAllowedOrigin(requestOrigin))
    }

    @ParameterizedTest
    @ValueSource(strings = ["https://example.com", "http://localhost:3000", "https://sub.domain.example.com:8443"])
    fun `given enableCors is enabled when the interceptor returns a MatchRequest strategy then the requested origin is reflected`(
        requestOrigin: String
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.MatchRequest()

        assertEquals(requestOrigin, requestAllowedOrigin(requestOrigin))
    }

    @ParameterizedTest
    @ValueSource(strings = ["https://a.example.com", "https://b.example.com"])
    fun `given enableCors is enabled when the interceptor returns a Specific strategy then an allowed origin is reflected`(
        requestOrigin: String
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(false, specificAllowedOrigins)

        assertEquals(requestOrigin, requestAllowedOrigin(requestOrigin))
    }

    @ParameterizedTest
    @ValueSource(strings = ["HTTPS://A.EXAMPLE.COM", "https://evil.example.com"])
    fun `given enableCors is enabled when the interceptor returns a Specific strategy then a disallowed origin yields no 'Access-Control-Allow-Origin' header`(
        requestOrigin: String
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(false, specificAllowedOrigins)

        assertNull(requestAllowedOrigin(requestOrigin))
    }

    // --- CORS handling of the non-controller dispatchers ---
    //
    // Dispatchers are invoked ascending by priority: AssetsDispatcher (-10), ServiceDispatcher (-5),
    // ControllerDispatcher (+10), HelpDispatcher (+100) and finally DefaultDispatcher (999). Since an origin may only
    // be resolved once per request, the first dispatcher which resolves one determines the outcome for the whole
    // request - the interceptors are only consulted if `ControllerDispatcher` gets there first.

    @Test
    fun `given enableCors is enabled when an asset is requested then the AssetsDispatcher allows any origin`() {
        val connection = sendGetTo("/assets/test.png", "https://example.com")

        assertEquals("*", connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
    }

    @Test
    fun `given an interceptor restricting the origin when an asset is requested then the AssetsDispatcher still allows any origin`() {
        // The AssetsDispatcher runs before the ControllerDispatcher, so its `Wildcard` is resolved first and wins.
        // Assets are public static files, hence they are deliberately not subject to the interceptors.
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(false, specificAllowedOrigins)

        val connection = sendGetTo("/assets/test.png", "https://untrusted.example.com")

        assertEquals("*", connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
    }

    @Test
    fun `given enableCors is enabled when the help system is requested then the requested origin is reflected`() {
        // Note that the HelpDispatcher (+100) declares a `Wildcard`, but it is invoked after the ControllerDispatcher
        // (+10), which already resolves an origin for every request carrying an `Origin` header. As only the first
        // resolution counts, the `Wildcard` never takes effect for an actual CORS request and the requested origin is
        // reflected instead. It only applies to requests without an `Origin` header - see the test below.
        val connection = sendGetTo("/help", "https://example.com")

        assertEquals(
            "https://example.com",
            connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
        )
    }

    @Test
    fun `given enableCors is enabled when the help system is requested without an origin then the HelpDispatcher allows any origin`() {
        // Without an `Origin` header the ControllerDispatcher skips the resolution entirely, so the HelpDispatcher is
        // the first to resolve one and its `Wildcard` wins.
        val connection = sendGetTo("/help", null)

        assertEquals("*", connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
    }

    @Test
    fun `given enableCors is enabled when an unknown path is requested then the requested origin is reflected`() {
        val connection = sendGetTo("/no-such-path", "https://example.com")

        assertEquals(
            "https://example.com",
            connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
        )
    }

    @Test
    fun `given an interceptor restricting the origin when an unknown path is requested then the DefaultDispatcher does not reflect a disallowed origin`() {
        // The ControllerDispatcher resolves the interceptor's strategy before falling through to the DefaultDispatcher,
        // so the latter must not be able to replace it with its own `MatchRequest`.
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(false, specificAllowedOrigins)

        val connection = sendGetTo("/no-such-path", "https://untrusted.example.com")

        assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
    }

    @Test
    fun `given enableCors is enabled when a service is requested then the requested origin is reflected`() {
        val connection = sendGetTo("/service/json/no-such-service", "https://example.com")

        assertEquals(
            "https://example.com",
            connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
        )
    }

    @Test
    fun `given an interceptor restricting the origin when an unknown service is requested then a disallowed origin is not reflected`() {
        // The ServiceDispatcher only resolves an origin once it actually handles the request. For an unknown service
        // it continues without resolving, so the ControllerDispatcher still gets to apply the interceptor's strategy
        // instead of the hardcoded `MatchRequest`.
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(false, specificAllowedOrigins)

        val connection = sendGetTo("/service/json/no-such-service", "https://untrusted.example.com")

        assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
    }

    // --- Access-Control-Allow-Credentials (CORS enabled) ---

    @ParameterizedTest
    @ValueSource(strings = ["https://a.example.com", "https://b.example.com"])
    fun `given enableCors is enabled when the interceptor allows credentials for a Specific origin then the credentials header is set`(
        requestOrigin: String
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(true, specificAllowedOrigins)

        val connection = sendGet(requestOrigin)

        assertAll(
            {
                assertEquals(
                    requestOrigin,
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
                )
            },
            {
                assertEquals(
                    "true",
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS.toString())
                )
            },
        )
    }

    @Test
    fun `given enableCors is enabled when the interceptor does not allow credentials for a Specific origin then no credentials header is set`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(false, specificAllowedOrigins)

        val connection = sendGet("https://a.example.com")

        assertAll(
            {
                assertEquals(
                    "https://a.example.com",
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
                )
            },
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS.toString())) },
        )
    }

    @Test
    fun `given enableCors is enabled when the interceptor reflects the origin (MatchRequest) then no credentials header is set`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.MatchRequest()

        val connection = sendGet("https://example.com")

        assertAll(
            {
                assertEquals(
                    "https://example.com",
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
                )
            },
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS.toString())) },
        )
    }

    @Test
    fun `given enableCors is enabled when the interceptor allows any origin (Wildcard) then no credentials header is set`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Wildcard()

        val connection = sendGet("https://example.com")

        assertAll(
            { assertEquals("*", connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())) },
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS.toString())) },
        )
    }

    @Test
    fun `given enableCors is enabled when the interceptor allows credentials for a Specific origin then a disallowed origin yields neither the origin nor the credentials header`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(true, specificAllowedOrigins)

        val connection = sendGet("https://evil.example.com")

        assertAll(
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())) },
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS.toString())) },
        )
    }

    @Test
    fun `given enableCors is enabled when a preflight allows credentials for a Specific origin then origin and credentials headers are set`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(true, specificAllowedOrigins)

        val connection = sendPreflight(origin = "https://b.example.com")

        assertAll(
            {
                assertEquals(
                    "https://b.example.com",
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
                )
            },
            {
                assertEquals(
                    "true",
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS.toString())
                )
            },
        )
    }

    @Test
    fun `given enableCors is enabled when a preflight does not allow credentials for a Specific origin then no credentials header is set`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(false, specificAllowedOrigins)

        val connection = sendPreflight(origin = "https://b.example.com")

        assertAll(
            {
                assertEquals(
                    "https://b.example.com",
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
                )
            },
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS.toString())) },
        )
    }

    // --- Vary (CORS enabled) ---
    //
    // The `Vary` header is checked for the `Origin` token rather than exact equality, so unrelated entries (e.g.
    // `Accept-Encoding` added by compression) do not affect the result.

    @Test
    fun `given enableCors is enabled when the origin is reflected (MatchRequest) then Vary lists Origin`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.MatchRequest()

        val vary = sendGet("https://example.com").getHeaderField(HttpHeaderNames.VARY.toString())
        assertAll(
            { assertNotNull(vary) },
            { assertContains(vary.orEmpty(), HttpHeaderNames.ORIGIN.toString(), ignoreCase = true) },
        )
    }

    @Test
    fun `given enableCors is enabled when a Specific origin is not allowed then Vary still lists Origin although no allow-origin header is returned`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(false, specificAllowedOrigins)

        val connection = sendGet("https://evil.example.com")
        val vary = connection.getHeaderField(HttpHeaderNames.VARY.toString())

        assertAll(
            { assertNull(connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())) },
            { assertNotNull(vary) },
            { assertContains(vary.orEmpty(), HttpHeaderNames.ORIGIN.toString(), ignoreCase = true) },
        )
    }

    @Test
    fun `given enableCors is enabled when the route already set a Vary header then Origin is appended instead of overriding it`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.MatchRequest()

        val connection = URI("http://localhost:9999/test/vary").toURL().openConnection() as HttpURLConnection
        connection.addRequestProperty(HttpHeaderNames.ORIGIN.toString(), "https://example.com")
        connection.inputStream.close()
        val vary = connection.getHeaderField(HttpHeaderNames.VARY.toString())

        assertAll(
            { assertNotNull(vary) },
            { assertContains(vary.orEmpty(), HttpHeaderNames.ACCEPT_ENCODING.toString(), ignoreCase = true) },
            { assertContains(vary.orEmpty(), HttpHeaderNames.ORIGIN.toString(), ignoreCase = true) },
        )
    }

    // --- Scope peeking (must not discard a manually installed user) ---

    @Test
    fun `a manually installed user survives the CORS origin resolution`() {
        // Resolving the origin happens very early in `ControllerDispatcher#dispatch()` - before the routes are matched
        // and their permissions are checked - and it needs the scope to evaluate `http.enableCors`. Reading the scope
        // via `UserContext#getScope()` would *install* it, and `setCurrentScope()` resets the current user. A user
        // installed manually (as tests and background jobs do) would therefore be discarded before the permission
        // check ran, turning a legitimate request into a permission error. `WebContext#isCorsEnabled()` uses
        // `UserContext#peekScope()` instead, which detects the scope without binding it.
        UserContext.get().setCurrentUser(
            UserInfo.Builder.createUser("test").withPermissions(setOf("permission-system-tags")).build()
        )

        val response = TestRequest.GET("/system/tags")
            .addHeader(HttpHeaderNames.ORIGIN, "https://example.com")
            .execute()

        // Note that the assertions below must inspect the request's own `CallContext` (via `getCallContext()`): the
        // pipeline dispatches on a forked task, so the `CorsContext` created while dispatching never appears in this
        // thread's context. For the same reason the surviving user cannot be asserted directly - `UserContext#fork()`
        // hands the forked task a *copy*, so this thread's user stays installed either way. The rendered page is the
        // observable proof that the permission check inside the request still saw the user.
        val corsContext = response.callContext.getOrCreateSubContext(CorsContext::class.java)

        assertAll(
            // The request must actually have gone through the CORS origin resolution, otherwise this test would
            // silently degrade into a duplicate of `PermissionsRouteTest` and no longer guard the regression.
            { assertEquals("https://example.com", corsContext.resolvedOrigin.orElse(null)) },
            // The permission check must have passed, rendering the real page instead of an error page.
            { assertEquals(TestResponse.ResponseType.TEMPLATE, response.type) },
            { assertEquals("/templates/system/tags.html.pasta", response.templateName) },
        )
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var corsAllowOriginResolver: CorsAllowOriginResolver
    }

    private fun configuredScope(scopeType: String, enableCors: Boolean?): ScopeInfo =
        ScopeInfo(
            scopeType,
            scopeType,
            scopeType,
            null,
            enableCors?.let { value -> { ConfigFactory.parseString("http.enableCors=$value") } },
            null
        )

    /**
     * Sends a GET request to `/system/ok` and returns the resulting `Access-Control-Allow-Origin` response header, or
     * `null` if none was set.
     */
    private fun requestAllowedOrigin(requestOrigin: String?, disableCorsAll: Boolean = false): String? =
        sendGet(requestOrigin, disableCorsAll).getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())

    /**
     * Sends a GET request to [uri], sending [requestOrigin] as `Origin` header if given, and returns the connection so
     * that the response headers can be inspected.
     *
     * In contrast to [sendGet] this also tolerates error responses (e.g. the 404 produced for an unknown path), which
     * expose their body via `errorStream` instead of `inputStream`.
     */
    private fun sendGetTo(uri: String, requestOrigin: String?): HttpURLConnection {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        if (requestOrigin != null) {
            connection.addRequestProperty(HttpHeaderNames.ORIGIN.toString(), requestOrigin)
        }
        runCatching { connection.inputStream.close() }.onFailure { connection.errorStream?.close() }
        return connection
    }

    /**
     * Sends a GET request to `/system/ok` and returns the connection so that the response headers can be inspected.
     *
     * If [requestOrigin] is given, it is sent as the `Origin` header. If [disableCorsAll] is set, the request is bound
     * (via [TestCorsScopeDetector]) to a scope which disables the automatic CORS handling.
     */
    private fun sendGet(requestOrigin: String?, disableCorsAll: Boolean = false): HttpURLConnection {
        val connection = URI("http://localhost:9999/system/ok").toURL().openConnection() as HttpURLConnection
        if (requestOrigin != null) {
            connection.addRequestProperty(HttpHeaderNames.ORIGIN.toString(), requestOrigin)
        }
        if (disableCorsAll) {
            connection.addRequestProperty(TestCorsScopeDetector.HEADER_DISABLE_CORS_ALL, "true")
        }
        connection.inputStream.close()
        return connection
    }

    /**
     * Sends a CORS preflight (`OPTIONS`) request to [uri] and returns the connection, so that the response headers can
     * be inspected.
     *
     * If [disableCorsAll] is set, the request is bound (via [TestCorsScopeDetector]) to a scope which disables the
     * automatic CORS handling.
     */
    private fun sendPreflight(
        origin: String,
        uri: String = "/system/ok",
        requestMethod: String = HttpMethod.GET.name(),
        requestHeaders: String? = null,
        disableCorsAll: Boolean = false
    ): HttpURLConnection {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = HttpMethod.OPTIONS.name()
        connection.addRequestProperty(HttpHeaderNames.ORIGIN.toString(), origin)
        connection.addRequestProperty(HttpHeaderNames.ACCESS_CONTROL_REQUEST_METHOD.toString(), requestMethod)
        if (requestHeaders != null) {
            connection.addRequestProperty(HttpHeaderNames.ACCESS_CONTROL_REQUEST_HEADERS.toString(), requestHeaders)
        }
        if (disableCorsAll) {
            connection.addRequestProperty(TestCorsScopeDetector.HEADER_DISABLE_CORS_ALL, "true")
        }
        connection.inputStream.close()
        return connection
    }
}
