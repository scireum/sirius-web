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
import sirius.kernel.async.CallContext
import sirius.web.http.TestRequest
import sirius.web.http.TestResponse

class NodeInfoServiceSpec extends BaseSpecification {

    def "/service/xml/system/node-info returns XML"() {
        when:
        def result = TestRequest.GET("/service/xml/system/node-info").execute()
        then:
        result.getStatus() == HttpResponseStatus.OK
        result.getType() == TestResponse.ResponseType.STREAM
        result.xmlContent().queryString("name") == CallContext.getCurrent().getNodeName()
    }

}