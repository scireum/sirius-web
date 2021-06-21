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
import sirius.kernel.commons.Context
import sirius.web.health.console.ConsoleController
import sirius.web.http.TestRequest
import sirius.web.http.TestResponse
import sirius.web.security.UserContext
import sirius.web.security.UserInfo
import sirius.web.util.JSONPath

class ConsoleControllerSpec extends BaseSpecification {

    def "/system/console renders its template"() {
        given:
        UserContext.get().setCurrentUser(UserInfo.Builder.createUser("test")
                                                 .withPermissions(Collections.
                                                         singleton(ConsoleController.PERMISSION_SYSTEM_CONSOLE)).build())
        when:
        def result = TestRequest.GET("/system/console").execute()
        then:
        result.getStatus() == HttpResponseStatus.OK
        result.getType() == TestResponse.ResponseType.TEMPLATE
        result.getTemplateName() == "/templates/system/console.html.pasta"
    }

    def "/system/console/api returns JSON for help"() {
        when:
        UserContext.get().setCurrentUser(UserInfo.
                Builder.
                createUser("test").
                withPermissions(Collections.singleton(ConsoleController.PERMISSION_SYSTEM_CONSOLE)).
                build())
        def result = TestRequest.
                POST("/system/console/api").
                withParameters(Context.create().set("command", "help")).
                execute()
        and:
        def json = result.getContentAsJson()
        then:
        result.getStatus() == HttpResponseStatus.OK
        JSONPath.queryValue(json, "error.code").isEmptyString()
        JSONPath.queryValue(json, "result").isFilled()
    }

}
