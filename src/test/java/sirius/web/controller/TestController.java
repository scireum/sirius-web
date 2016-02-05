/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.WebContext;
import sirius.web.services.JSONStructuredOutput;
import sirius.web.templates.Resources;

import java.io.IOException;
import java.io.OutputStream;

@Register
public class TestController implements Controller {
    @Override
    public void onError(WebContext ctx, HandledException error) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error.getMessage());
    }

    @Routed("/test/post")
    public void postTest(WebContext ctx) {
        ctx.respondWith()
           .setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/plain")
           .direct(HttpResponseStatus.OK, ctx.get("value").asString());
    }

    @Routed(value = "/test/json", jsonCall = true)
    public void testJSON(WebContext ctx, JSONStructuredOutput out) {
        out.property("test", ctx.getParameter("test"));
    }

    @Routed(value = "/test/json-param/:1", jsonCall = true)
    public void testJSONParam(WebContext ctx, JSONStructuredOutput out, String param) {
        out.property("test", param);
    }

    @Routed(value = "/test/params/:2/:1", jsonCall = true)
    public void testJSONParams(WebContext ctx, JSONStructuredOutput out, String param1, String param2) {
        out.property("param1", param1);
        out.property("param2", param2);
    }

    @Routed("/tunnel/test")
    public void tunnelTest(WebContext ctx) {
        ctx.respondWith()
           .setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/test")
           .tunnel("http://localhost:9999/service/json/test");
    }

    @Routed("/tunnel/test_large")
    public void tunnelTestLarge(WebContext ctx) {
        ctx.respondWith().tunnel("http://localhost:9999/service/json/test_large");
    }

    @Routed(value = "/test/predispatch", preDispatchable = true)
    public void testPredispatch(WebContext ctx, InputStreamHandler in) throws Exception {
        int size = 0;
        while (in.read() >= 0) {
            size++;
        }
        in.close();
        ctx.respondWith().direct(HttpResponseStatus.OK, String.valueOf(size));
    }

    @Routed("/test/os")
    public void testOutputStream(WebContext ctx) throws IOException {
        OutputStream out = ctx.respondWith().outputStream(HttpResponseStatus.OK, "text/plain");
        byte[] buffer = new byte[8192];
        for (int i = 0; i < 9; i++) {
            out.write(buffer, 0, 8192);
        }
        out.close();
    }

    @Part
    private Resources res;

    @Routed("/test/resource")
    public void testResource(WebContext ctx) throws IOException {
        ctx.respondWith().resource(res.resolve("assets/test_large.css").get().getUrl().openConnection());
    }

    @Routed("/test/resource_uncompressable")
    public void testResourceUncompressable(WebContext ctx) throws IOException {
        ctx.respondWith().named("test_large.jpg").resource(res.resolve("assets/test_large.css").get().getUrl().openConnection());
    }

}
