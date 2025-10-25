/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.cookie.CookieHeaderNames;
import sirius.kernel.async.Future;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Streams;
import sirius.kernel.commons.Wait;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.web.http.CSRFHelper;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.Limited;
import sirius.web.http.WebContext;
import sirius.web.resources.Resources;
import sirius.web.services.InternalService;
import sirius.web.services.JSONStructuredOutput;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.List;
import java.util.Optional;
import java.util.zip.GZIPInputStream;

/**
 * Provides various test endpoints used by the unit and integration tests.
 */
@Register
public class TestController extends BasicController {

    @Part
    private Tasks tasks;

    @Part
    private CSRFHelper csrfHelper;

    @Part
    private Resources resources;

    @Override
    public void onError(WebContext webContext, HandledException error) {
        webContext.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error.getMessage());
    }

    @Routed("/test/post")
    public void postTest(WebContext webContext) {
        webContext.respondWith()
                  .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/plain")
                  .direct(HttpResponseStatus.OK, webContext.get("value").asString());
    }

    @InternalService
    @Routed("/test/json")
    public void testJSON(WebContext webContext, JSONStructuredOutput out) {
        out.property("test", webContext.getParameter("test"));
    }

    @Routed("/rewrite")
    public void testRewriting(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/test/cookieCacheTest")
    public void testCookieCacheTest(WebContext webContext) {
        webContext.setCookie("Test", "1", 3600, CookieHeaderNames.SameSite.Strict, WebContext.CookieSecurity.IF_SSL);
        webContext.respondWith().cached().direct(HttpResponseStatus.OK, "OK");
    }

    @InternalService
    @Routed("/test/json-param/:1")
    public void testJSONParam(WebContext webContext, JSONStructuredOutput output, String parameter) {
        output.property("test", parameter);
    }

    @InternalService
    @Routed("/test/json-params/:1/:2")
    public void testJSONParams(WebContext webContext,
                               JSONStructuredOutput output,
                               String parameter1,
                               String parameter2) {
        output.property("param1", parameter1);
        output.property("param2", parameter2);
    }

    @InternalService
    @Routed("/test/mixed-json-params/:2/:1")
    public void testMixedJSONParams(WebContext webContext,
                                    JSONStructuredOutput output,
                                    String parameter1,
                                    String parameter2) {
        output.property("param1", parameter1);
        output.property("param2", parameter2);
    }

    @InternalService
    @Routed("/test/json-params-varargs/:1/:2/**")
    public void testJSONWithVarArgs(WebContext webContext,
                                    JSONStructuredOutput output,
                                    String parameter1,
                                    String parameter2,
                                    List<String> parameterList) {
        output.property("param1", parameter1);
        output.property("param2", parameter2);
        output.array("params", "param", parameterList);
    }

    @Routed("/tunnel/test")
    public void tunnelTest(WebContext webContext) {
        webContext.respondWith()
                  .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
                  .tunnel("http://localhost:9999/api/test");
    }

    @Routed("/tunnel/test/tune")
    public void tunnelTuneTest(WebContext webContext) {
        webContext.respondWith()
                  .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
                  .tunnel("http://localhost:9999/tunnel/test/tune/target",
                          request -> request.setMethod("POST"),
                          null,
                          null);
    }

    @Routed("/tunnel/test/tune/target")
    public void tunnelTuneTestTarget(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, webContext.getRequest().method().name());
    }

    @Routed("/tunnel/fallback_for_404")
    public void tunnelFallbackFor404(WebContext webContext) {
        webContext.respondWith()
                  .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
                  .tunnel("http://localhost:9999/api/DOES_NOT_EXIST",
                          code -> webContext.respondWith()
                                            .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
                                            .tunnel("http://localhost:9999/api/test"));
    }

    @Routed("/tunnel/fallback_for_error")
    public void tunnelFallbackForError(WebContext webContext) {
        webContext.respondWith()
                  .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
                  .tunnel("http://localhost:1",
                          code -> webContext.respondWith()
                                            .setHeader(HttpHeaderNames.CONTENT_TYPE, "text/test")
                                            .tunnel("http://localhost:9999/api/test"));
    }

    @Routed("/tunnel/test_large")
    public void tunnelTestLarge(WebContext webContext) {
        webContext.respondWith().tunnel("http://localhost:9999/api/test/test_large");
    }

    @Routed("/tunnel/test_transform")
    public void tunnelTestTransform(WebContext webContext) {
        webContext.respondWith().tunnel("http://localhost:9999/api/test/test_large", buffer -> {
            if (buffer.readableBytes() == 0) {
                return Optional.empty();
            }

            byte[] array = buffer.array();
            // Transform all bytes by + 1
            for (int i = 0; i < array.length; i++) {
                array[i] = (byte) ((array[i] + 1) % 255);
            }

            buffer.retain();
            return Optional.of(buffer);
        }, null);
    }

    @Routed(value = "/test/predispatch", preDispatchable = true)
    public void testPredispatch(WebContext webContext, InputStreamHandler input) throws Exception {
        int size = 0;
        while (input.read() >= 0) {
            size++;
        }
        input.close();
        webContext.respondWith().direct(HttpResponseStatus.OK, String.valueOf(size));
    }

    @Routed(value = "/test/predispatch/abort", preDispatchable = true)
    public void testPredispatchAbort(WebContext webContext, InputStreamHandler input) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "ABORT");
    }

    @Routed("/test/os")
    public void testOutputStream(WebContext webContext) throws IOException {
        OutputStream output = webContext.respondWith().outputStream(HttpResponseStatus.OK, "text/plain");
        byte[] buffer = new byte[8192];
        for (int i = 0; i < 9; i++) {
            output.write(buffer, 0, 8192);
        }
        output.close();
    }

    @Routed("/test/resource")
    public void testResource(WebContext webContext) throws IOException {
        webContext.respondWith().resource(resources.resolve("assets/test_large.css").get().getUrl().openConnection());
    }

    @Routed("/test/resource_uncompressable")
    public void testResourceUncompressable(WebContext webContext) throws IOException {
        webContext.respondWith()
                  .named("test_large.jpg")
                  .resource(resources.resolve("assets/test_large.css").get().getUrl().openConnection());
    }

    @InternalService
    @Routed(value = "/upload-test", preDispatchable = true)
    public void uploadTest(WebContext webContext, JSONStructuredOutput output, InputStreamHandler upload)
            throws IOException {
        try (upload) {
            long size = Streams.exhaust(upload);
            output.property("size", size);
        }
    }

    @InternalService
    @Routed(value = "/upload-gzip", preDispatchable = true)
    public void uploadGzipTest(WebContext webContext, JSONStructuredOutput output, InputStreamHandler upload)
            throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(upload)))) {
            output.property("lines", reader.lines().count());
        }
    }

    @Routed("/test/firewall")
    @Limited
    public void firewallTest(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/test/firewallBlocked")
    @Limited("blocked")
    public void firewallTestBlocked(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/test/fake-delete-data")
    public void deleteData(WebContext webContext) {
        if (!webContext.isSafePOST()) {
            webContext.respondWith().status(HttpResponseStatus.INTERNAL_SERVER_ERROR);
            return;
        }
        webContext.respondWith().status(HttpResponseStatus.OK);
    }

    @Routed("/test/fake-delete-data-unsafe")
    public void deleteDataUnsafe(WebContext webContext) {
        if (!webContext.isUnsafePOST()) {
            webContext.respondWith().status(HttpResponseStatus.INTERNAL_SERVER_ERROR);
            return;
        }
        webContext.respondWith().status(HttpResponseStatus.OK);
    }

    @Routed("/test/fake-delete-data-ensure-safe")
    public void deleteDataEnsureSafe(WebContext webContext) {
        try {
            if (!webContext.ensureSafePOST()) {
                webContext.respondWith().status(HttpResponseStatus.INTERNAL_SERVER_ERROR);
                return;
            }
        } catch (HandledException _) {
            webContext.respondWith().status(HttpResponseStatus.UNAUTHORIZED);
            return;
        }

        webContext.respondWith().status(HttpResponseStatus.OK);
    }

    @Routed("/test/provide-security-token")
    public void provideSecurityToken(WebContext webContext) {
        webContext.respondWith().template("/templates/security-token.html.pasta");
    }

    @Routed("/test/expire-security-token")
    public void expireSecurityToken(WebContext webContext) {
        csrfHelper.recomputeCSRFToken(webContext);
        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @InternalService
    @Routed("/test/json/async")
    public Future asyncJSONCall(WebContext webContext, JSONStructuredOutput output) {
        return tasks.defaultExecutor().start(() -> {
            Wait.seconds(1);
            output.property("test", "1");
        });
    }

    @Routed("/test/session-test")
    public void sessionTest(WebContext webContext) {
        webContext.setSessionValue("test1", "test");
        webContext.setSessionValue("test2", null);

        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/test/redirect-to-get")
    public void redirect(WebContext webContext) {
        webContext.respondWith().redirectToGet("/test/redirect-target");
    }

    @Routed("/test/redirect-target")
    public void redirectTarget(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "target");
    }
}
