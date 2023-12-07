/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.web.http.TestRequest
import sirius.web.http.TestResponse
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class PermissionAnnotationTest {

    @Test
    fun `user can access 'system tags', when permission is given`() {
        UserContext.get().setCurrentUser(
            UserInfo.Builder.createUser("test").withPermissions(setOf("permission-system-tags")).build()
        )
        val response = TestRequest.GET("/system/tags").execute()
        assertEquals(TestResponse.ResponseType.TEMPLATE, response.getType())
        assertEquals("/templates/system/tags.html.pasta", response.getTemplateName())
    }

    @Test
    fun `user can't access 'system tags', when permission is not given`() {
        UserContext.get().setCurrentUser(
            UserInfo.Builder.createUser("test").withPermissions(setOf()).build()
        )
        val response = TestRequest.GET("/system/tags").execute()
        assertEquals(TestResponse.ResponseType.ERROR, response.getType())
    }

}
