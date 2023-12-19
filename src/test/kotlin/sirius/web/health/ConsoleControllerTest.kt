/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health

import com.fasterxml.jackson.core.JsonPointer
import io.netty.handler.codec.http.HttpResponseStatus
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Context
import sirius.kernel.commons.Json
import sirius.web.health.console.ConsoleController
import sirius.web.http.TestRequest
import sirius.web.http.TestResponse
import sirius.web.security.UserContext
import sirius.web.security.UserInfo
import kotlin.test.assertEquals
import java.util.Collections

@ExtendWith(SiriusExtension::class)
class ConsoleControllerTest {
    @Test
    fun `System console renders its template`() {
        UserContext.get().setCurrentUser(UserInfo.Builder.createUser("test")
            .withPermissions(Collections.
            singleton(ConsoleController.PERMISSION_SYSTEM_CONSOLE))
            .build())
        val result = TestRequest.GET("/system/console").execute()
        assertEquals(HttpResponseStatus.OK,result.getStatus())
        assertEquals(TestResponse.ResponseType.TEMPLATE, result.getType())
        assertEquals("/templates/system/console.html.pasta", result.getTemplateName())
    }

    @Test
    fun `System console API returns JSON for help`() {
        UserContext.get().setCurrentUser(UserInfo.
        Builder.
        createUser("test").
        withPermissions(Collections.singleton(ConsoleController.PERMISSION_SYSTEM_CONSOLE)).
        build())
        val result = TestRequest.
        POST("/system/console/api").
        withParameters(Context.create().set("command", "help")).
        execute()
        val json = result.getContentAsJson()
        assertEquals(HttpResponseStatus.OK,result.getStatus())
        assertTrue{Json.tryGetAt(json, JsonPointer.compile("/error/code"))
            .map { Json.convertToValue(it) }.map { it.isEmptyString()}.orElse(true) }
        assertTrue{ Json.tryGetAt(json, JsonPointer.compile("/result"))
            .map { Json.convertToValue(it) }.map { it.isFilled() }.orElse(false)}
    }
}
