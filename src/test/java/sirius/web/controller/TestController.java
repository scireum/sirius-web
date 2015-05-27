/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.web.http.WebContext;

@Register
public class TestController implements Controller {
    @Override
    public void onError(WebContext ctx, HandledException error) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error.getMessage());
    }

    @Routed("/tunnel/test")
    public void tunnelTest(WebContext ctx) {
        ctx.respondWith().tunnel("http://localhost:9999/service/json/test");
    }

    @Routed("/tunnel/test_large")
    public void tunnelTestLarge(WebContext ctx) {
        ctx.respondWith().tunnel("http://localhost:9999/service/json/test_large");
    }
}
