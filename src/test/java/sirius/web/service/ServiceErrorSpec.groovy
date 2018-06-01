package sirius.web.service

import io.netty.handler.codec.http.HttpResponseStatus
import sirius.kernel.BaseSpecification
import sirius.web.http.TestRequest

class ServiceErrorSpec extends BaseSpecification {

    def "default error service handling"() {
        when:
        def result = TestRequest.GET("/service/xml/test/default-error").execute()
        then:
        result.getStatus() == HttpResponseStatus.OK
        and:
        result.xmlContent().queryValue("error").asBoolean()
        !result.xmlContent().queryValue("success").asBoolean()
        and:
        !result.xmlContent().hasProperty("type")
    }

    def "special error service handling"() {
        when:
        def result = TestRequest.GET("/service/xml/test/special-error").execute()
        then:
        result.getStatus() == HttpResponseStatus.OK
        and:
        !result.xmlContent().queryValue("error").asBoolean()
        !result.xmlContent().queryValue("success").asBoolean()
        and:
        result.xmlContent().queryValue("type").asString() == "special error service"

    }
}
