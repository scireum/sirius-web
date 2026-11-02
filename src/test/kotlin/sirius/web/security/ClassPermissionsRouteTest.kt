/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import io.netty.handler.codec.http.HttpResponseStatus
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.web.http.TestRequest
import sirius.web.http.TestResponse
import sirius.web.http.WebContext
import kotlin.test.assertEquals

/**
 * Verifies that a [Permission] placed on a controller class applies to all of its routes, in addition to the
 * permissions of the respective method.
 */
@ExtendWith(SiriusExtension::class)
class ClassPermissionsRouteTest {

    private fun installUser(vararg permissions: String) {
        UserContext.get().setCurrentUser(
            UserInfo.Builder.createUser("test").withPermissions(permissions.toSet()).build()
        )
    }

    @Test
    fun `route is rejected, when the permission of the controller class is missing`() {
        installUser()

        val response = TestRequest.GET("/test/class-permission").execute()

        assertEquals(TestResponse.ResponseType.ERROR, response.getType())
    }

    @Test
    fun `route is executed, when the permission of the controller class is given`() {
        installUser(ClassPermissionTestController.CLASS_PERMISSION)

        val response = TestRequest.GET("/test/class-permission").execute()

        assertEquals(HttpResponseStatus.OK, response.getStatus())
        assertEquals("OK", response.getContentAsString())
    }

    @Test
    fun `route requires both, the permission of the controller class and the one of the method`() {
        installUser(ClassPermissionTestController.METHOD_PERMISSION)
        assertEquals(
            TestResponse.ResponseType.ERROR,
            TestRequest.GET("/test/class-and-method-permission").execute().getType()
        )

        installUser(ClassPermissionTestController.CLASS_PERMISSION)
        assertEquals(
            TestResponse.ResponseType.ERROR,
            TestRequest.GET("/test/class-and-method-permission").execute().getType()
        )

        installUser(ClassPermissionTestController.CLASS_PERMISSION, ClassPermissionTestController.METHOD_PERMISSION)
        assertEquals(
            HttpResponseStatus.OK,
            TestRequest.GET("/test/class-and-method-permission").execute().getStatus()
        )
    }

    @Test
    fun `permissions of the type and the method are combined`() {
        val method = ClassPermissionTestController::class.java.getMethod(
            "classAndMethodPermission",
            WebContext::class.java
        )

        assertEquals(
            setOf(ClassPermissionTestController.CLASS_PERMISSION, ClassPermissionTestController.METHOD_PERMISSION),
            Permissions.computePermissionsFromAnnotations(ClassPermissionTestController::class.java, method)
        )
        assertEquals(
            setOf(ClassPermissionTestController.METHOD_PERMISSION),
            Permissions.computePermissionsFromAnnotations(method)
        )
    }
}
