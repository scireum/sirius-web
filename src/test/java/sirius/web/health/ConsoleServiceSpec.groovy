/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */


package sirius.web.health

import com.alibaba.fastjson.JSONObject
import com.google.common.collect.Lists
import io.netty.handler.codec.http.HttpResponseStatus
import sirius.kernel.BaseSpecification
import sirius.web.http.TestRequest
import sirius.web.security.UserContext
import sirius.web.security.UserInfo

class ConsoleServiceSpec extends BaseSpecification {

    def "/service/xml/console returns XML for help"() {
        when:
        UserContext.get().setCurrentUser(UserInfo.GOD_LIKE);
        JSONObject data = new JSONObject();
        data.put("method", "help");
        data.put("params", Lists.newArrayList());
        def result = TestRequest.POST("/service/xml/system/console", data).executeAndBlock();
        then:
        result.getStatus() == HttpResponseStatus.OK;
        result.xmlContent().queryString("error/code") == null
        result.xmlContent().queryString("result") != null
    }

}