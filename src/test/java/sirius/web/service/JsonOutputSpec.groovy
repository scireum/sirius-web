/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.service

import groovy.json.JsonSlurper
import sirius.kernel.BaseSpecification
import sirius.kernel.xml.Attribute
import sirius.web.services.JSONStructuredOutput

class JsonOutputSpec extends BaseSpecification {
    def "json output uses attributes"() {
        given:
        OutputStream os = new ByteArrayOutputStream();
        JSONStructuredOutput out = new JSONStructuredOutput(os, null, "UTF8");
        when:
        out.beginResult("test");
        out.beginObject("1", Attribute.set("a", "b"))
        out.endObject()
        out.endResult();
        then:
        compareJsonString(os.toString(), """{"1":{"a":"b"}}""");
    }

    private def compareJsonString(String str1, String str2) {
        return new JsonSlurper().parseText(str1) == new JsonSlurper().parseText(str2);
    }
}
