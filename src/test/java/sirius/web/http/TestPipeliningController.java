/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Explain;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;

@Register
public class TestPipeliningController extends BasicController {

    @Override
    public void onError(WebContext webContext, HandledException error) {
        throw new IllegalStateException();
    }

    @Routed("/pipelining/:1")
    @SuppressWarnings("squid:S2925")
    @Explain("We need this delay here to properly test pipelining.")
    public void test(WebContext ctx, String time) throws Exception {
        Thread.sleep(Long.parseLong(time));
        ctx.respondWith().addHeader("URI", ctx.getRequestedURI()).direct(HttpResponseStatus.OK, "OK");
    }
}
