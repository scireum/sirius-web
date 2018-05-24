/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import com.google.common.base.Charsets;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Register;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;

import java.io.OutputStream;
import java.util.function.Consumer;

@Register
public class TestDispatcher implements WebDispatcher {

    @Override
    public int getPriority() {
        return 100;
    }

    @Override
    public void dispatch(WebContext ctx, Consumer<WebContext> startOfPipeline, Consumer<WebContext> nextStage)
            throws Exception {
        if ("/large-blocking-calls".equalsIgnoreCase(ctx.getRequestedURI())) {
            // See WebServerSepc->"Invoke /large-blocking-calls with GET" to the appropriate test and explanation...
            OutputStream out = ctx.respondWith().outputStream(HttpResponseStatus.OK, "text/plain");
            for(int i = 0; i < 10000000; i++) {
                out.write("THISISLARGECONTENT".getBytes(Charsets.UTF_8));
            }
            out.close();
            return;
        }
        if ("/redispatch".equalsIgnoreCase(ctx.getRequestedURI())) {
            // See WebServerSepc->"Redispatching works" to the appropriate test and explanation...
            ctx.withCustomPath("/service/json/test");
            startOfPipeline.accept(ctx);
        } else {
            nextStage.accept(ctx);
        }
    }

    @Override
    public boolean dispatch(WebContext ctx) throws Exception {
        return false;
    }
}
