/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import com.google.common.io.ByteStreams;
import io.netty.buffer.Unpooled;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.Future;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Wait;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.web.http.CSRFHelper;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.Limited;
import sirius.web.http.WebContext;
import sirius.web.resources.Resources;
import sirius.web.services.JSONStructuredOutput;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.List;
import java.util.Optional;
import java.util.zip.GZIPInputStream;

@Register
public class TestController implements Controller {

    @Part
    private Tasks tasks;

    @Part
    private CSRFHelper csrfHelper;

    @Override
    public void onError(WebContext ctx, HandledException error) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error.getMessage());
    }

    @Routed("/test/post")
    public void postTest(WebContext ctx) {
        ctx.respondWith()
           .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/plain")
           .direct(HttpResponseStatus.OK, ctx.get("value").asString());
    }

    @Routed(value = "/test/json", jsonCall = true)
    public void testJSON(WebContext ctx, JSONStructuredOutput out) {
        out.property("test", ctx.getParameter("test"));
    }

    @Routed("/rewrite")
    public void testReqriting(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/test/cookieCacheTest")
    public void testCookieCacheTest(WebContext ctx) {
        ctx.setCookie("Test", "1", 3600);
        ctx.respondWith().cached().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed(value = "/test/json-param/:1", jsonCall = true)
    public void testJSONParam(WebContext ctx, JSONStructuredOutput out, String param) {
        out.property("test", param);
    }

    @Routed(value = "/test/json-params/:1/:2", jsonCall = true)
    public void testJSONParams(WebContext ctx, JSONStructuredOutput out, String param1, String param2) {
        out.property("param1", param1);
        out.property("param2", param2);
    }

    @Routed(value = "/test/mixed-json-params/:2/:1", jsonCall = true)
    public void testMixedJSONParams(WebContext ctx, JSONStructuredOutput out, String param1, String param2) {
        out.property("param1", param1);
        out.property("param2", param2);
    }

    @Routed(value = "/test/json-params-varargs/:1/:2/**", jsonCall = true)
    public void testJSONWithVarArgs(WebContext ctx,
                                    JSONStructuredOutput out,
                                    String param1,
                                    String param2,
                                    List<String> paramList) {
        out.property("param1", param1);
        out.property("param2", param2);
        out.array("params", "param", paramList);
    }

    @Routed("/tunnel/test")
    public void tunnelTest(WebContext ctx) {
        ctx.respondWith()
           .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
           .tunnel("http://localhost:9999/service/json/test");
    }

    @Routed("/tunnel/fallback_for_404")
    public void tunnelFallbackFor404(WebContext ctx) {
        ctx.respondWith()
           .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
           .tunnel("http://localhost:9999/service/json/DOES_NOT_EXIST",
                   (code) -> ctx.respondWith()
                                .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
                                .tunnel("http://localhost:9999/service/json/test"));
    }

    @Routed("/tunnel/fallback_for_error")
    public void tunnelFallbackForError(WebContext ctx) {
        ctx.respondWith()
           .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
           .tunnel("http://localhost:1",
                   (code) -> ctx.respondWith()
                                .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
                                .tunnel("http://localhost:9999/service/json/test"));
    }

    @Routed("/tunnel/test_large")
    public void tunnelTestLarge(WebContext ctx) {
        ctx.respondWith().tunnel("http://localhost:9999/service/json/test_large");
    }

    @Routed("/tunnel/test_transform")
    public void tunnelTestTransform(WebContext ctx) {
        ctx.respondWith().tunnel("http://localhost:9999/service/json/test_large", buf -> {
            if (buf.readableBytes() == 0) {
                return Optional.empty();
            }

            byte[] array = buf.array();
            // Transform all bytes by + 1
            for (int i = 0; i < array.length; i++) {
                array[i] = (byte) ((array[i] + 1) % 255);
            }

            buf.retain();
            return Optional.of(buf);
        }, null);
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

    @Routed(value = "/test/predispatch/abort", preDispatchable = true)
    public void testPredispatchAbort(WebContext ctx, InputStreamHandler in) throws Exception {
        ctx.respondWith().direct(HttpResponseStatus.OK, "ABORT");
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
        ctx.respondWith()
           .named("test_large.jpg")
           .resource(res.resolve("assets/test_large.css").get().getUrl().openConnection());
    }

    @Routed(value = "/upload-test", preDispatchable = true, jsonCall = true)
    public void uploadTest(WebContext ctx, JSONStructuredOutput out, InputStreamHandler upload) throws IOException {
        try {
            long size = ByteStreams.exhaust(upload);
            out.property("size", size);
        } finally {
            upload.close();
        }
    }

    @Routed(value = "/upload-gzip", preDispatchable = true, jsonCall = true)
    public void uploadGzipTest(WebContext ctx, JSONStructuredOutput out, InputStreamHandler upload) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(upload)))) {
            out.property("lines", reader.lines().count());
        }
    }

    @Routed("/test/firewall")
    @Limited
    public void firewallTest(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/test/firewallBlocked")
    @Limited("blocked")
    public void firewallTestBlocked(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/test/fake-delete-data")
    public void deleteData(WebContext ctx) {
        if (!ctx.isSafePOST()) {
            ctx.respondWith().status(HttpResponseStatus.INTERNAL_SERVER_ERROR);
            return;
        }
        ctx.respondWith().status(HttpResponseStatus.OK);
    }

    @Routed("/test/fake-delete-data-unsafe")
    public void deleteDataUnsafe(WebContext ctx) {
        if (!ctx.isUnsafePOST()) {
            ctx.respondWith().status(HttpResponseStatus.INTERNAL_SERVER_ERROR);
            return;
        }
        ctx.respondWith().status(HttpResponseStatus.OK);
    }

    @Routed("/test/fake-delete-data-ensure-safe")
    public void deleteDataEnsureSafe(WebContext ctx) {
        try {
            if (!ctx.ensureSafePOST()) {
                ctx.respondWith().status(HttpResponseStatus.INTERNAL_SERVER_ERROR);
                return;
            }
        } catch (HandledException e) {
            ctx.respondWith().status(HttpResponseStatus.UNAUTHORIZED);
            return;
        }

        ctx.respondWith().status(HttpResponseStatus.OK);
    }

    @Routed("/test/provide-security-token")
    public void provideSecurityToken(WebContext ctx) {
        ctx.respondWith().template("/templates/security-token.html.pasta");
    }

    @Routed("/test/expire-security-token")
    public void expireSecurityToken(WebContext ctx) {
        csrfHelper.recomputeCSRFToken(ctx);
        ctx.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed(value = "/test/json/async", jsonCall = true)
    public Future asyncJSONCall(WebContext ctx, JSONStructuredOutput out) {
        return tasks.defaultExecutor().start(() -> {
            Wait.seconds(1);
            out.property("test", "1");
        });
    }

    @Routed("/test/session-test")
    public void sessionTest(WebContext ctx) {
        ctx.setSessionValue("test1", "test");
        ctx.setSessionValue("test2", null);

        ctx.respondWith().direct(HttpResponseStatus.OK, "OK");
    }
}
