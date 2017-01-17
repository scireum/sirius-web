/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health

import io.netty.handler.codec.http.HttpResponseStatus
import sirius.kernel.BaseSpecification
import sirius.kernel.commons.Value
import sirius.kernel.di.Injector
import sirius.web.http.TestRequest
import sirius.web.http.TestResponse
import sirius.web.security.UserContext
import sirius.web.security.UserInfo

class SystemControllerSpec extends BaseSpecification {

    def "/system/ok returns 200 OK"() {
        when:
        def result = TestRequest.GET("/system/ok").executeAndBlock()
        then:
        result.getStatus() == HttpResponseStatus.OK
    }

    def "/system/console renders its template"() {
        given:
        UserContext.get().setCurrentUser(UserInfo.Builder.createUser("test")
                .withPermissions(Collections.singleton(SystemController.PERMISSION_SYSTEM_CONSOLE)).build())
        when:
        def result = TestRequest.GET("/system/console").executeAndBlock()
        then:
        result.getStatus() == HttpResponseStatus.OK
        result.getType() == TestResponse.ResponseType.TEMPLATE
        result.getTemplateName() == "/view/system/console.html"
    }

    def "/system/state renders its template"() {
        given:
        UserContext.get().setCurrentUser(UserInfo.Builder.createUser("test")
                .withPermissions(Collections.singleton(SystemController.PERMISSION_SYSTEM_STATE)).build())
        when:
        def result = TestRequest.GET("/system/state").executeAndBlock()
        then:
        result.getStatus() == HttpResponseStatus.OK
        result.getType() == TestResponse.ResponseType.TEMPLATE
        result.getTemplateName() == "/view/system/state.html"
        Value.indexOf(0, result.getTemplateParameters()).get() == Injector.context().getPart(Cluster.class)
    }

    def "/system/info renders its template"() {
        when:
        def result = TestRequest.GET("/system/info").executeAndBlock()
        then:
        result.getStatus() == HttpResponseStatus.OK
        result.getType() == TestResponse.ResponseType.TEMPLATE
        result.getTemplateName() == "/view/system/info.html"
    }

}