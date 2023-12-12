/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health

import io.netty.handler.codec.http.HttpResponseStatus
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.BaseSpecification
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Value
import sirius.kernel.di.Injector
import sirius.web.http.TestRequest
import sirius.web.http.TestResponse
import sirius.web.security.UserContext
import sirius.web.security.UserInfo
import java.util.*
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class SystemControllerTest {

    @Test
    fun ` system ok returns 200 OK`() {
        val result = TestRequest.GET("/system/ok").execute()
        assertEquals(HttpResponseStatus.OK, result.getStatus())
    }

    @Test
    fun ` system state renders its template`() {
        UserContext.get().setCurrentUser(
            UserInfo.Builder.createUser("test")
                .withPermissions(Collections.singleton(SystemController.PERMISSION_SYSTEM_STATE)).build()
        )
        val result = TestRequest.GET("/system/state").execute()
        assertEquals(HttpResponseStatus.OK, result.getStatus())
        assertEquals(TestResponse.ResponseType.TEMPLATE, result.getType())
        assertEquals("/templates/system/state.html.pasta", result.getTemplateName())
        Value.indexOf(0, result.getTemplateParameters()).get() == Injector.context().getPart(Cluster::class.java)
    }

    @Test
    fun ` system info renders its template`() {
        val result = TestRequest.GET("/system/info").execute()
        assertEquals(HttpResponseStatus.OK, result.getStatus())
        assertEquals(TestResponse.ResponseType.TEMPLATE, result.getType())
        result.getTemplateName() == "/templates/system/info.html.pasta"
    }

}
