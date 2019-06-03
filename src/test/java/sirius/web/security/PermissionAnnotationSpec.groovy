/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.kernel.BaseSpecification
import sirius.web.http.TestRequest
import sirius.web.http.TestResponse


class PermissionAnnotationSpec extends BaseSpecification {

    def "user can access '/system/tags', when permission is given"() {
        given:
        UserContext.get().
                setCurrentUser(UserInfo.Builder.createUser("test").
                        withPermissions(["permission-system-tags"] as Set).build())
        when:
        def response = TestRequest.GET("/system/tags").execute()
        then:
        response.getType() == TestResponse.ResponseType.TEMPLATE
        response.getTemplateName() == "/templates/system/tags.html.pasta"
    }

    def "user can't access '/system/tags', when permission is not given"() {
        given:
        UserContext.get().
                setCurrentUser(UserInfo.Builder.createUser("test").
                        withPermissions([] as Set).build())
        when:
        def response = TestRequest.GET("/system/tags").execute()
        then:
        response.getType() == TestResponse.ResponseType.ERROR
    }

}
