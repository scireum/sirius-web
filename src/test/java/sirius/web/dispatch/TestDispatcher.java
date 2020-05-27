/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Register;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;

import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

@Register
public class TestDispatcher implements WebDispatcher {

    @Override
    public int getPriority() {
        return 100;
    }

    @Override
    public DispatchDecision dispatch(WebContext ctx) throws Exception {
        if ("/large-blocking-calls".equalsIgnoreCase(ctx.getRequestedURI())) {
            // See WebServerSpec->"Invoke /large-blocking-calls with GET" to the appropriate test and explanation...
            OutputStream out = ctx.respondWith().outputStream(HttpResponseStatus.OK, "text/plain");
            for (int i = 0; i < 10000000; i++) {
                out.write("THISISLARGECONTENT".getBytes(StandardCharsets.UTF_8));
            }
            out.close();
            return DispatchDecision.DONE;
        }
        if ("/redispatch".equalsIgnoreCase(ctx.getRequestedURI())) {
            // See WebServerSepc->"Redispatching works" to the appropriate test and explanation...
            ctx.withCustomPath("/service/json/test");
            return DispatchDecision.RESTART;
        } else {
            return DispatchDecision.CONTINUE;
        }
    }
}
