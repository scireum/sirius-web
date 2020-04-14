/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;

@Register
public class CompletionPromiseTestController implements Controller {

    public static int lastPromisedReturnCode = 0;

    @Override
    public void onError(WebContext ctx, HandledException error) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error);
    }

    @Routed("/test/completion-promise")
    public void completePromise(WebContext context) {
        context.getCompletionPromise().onSuccess(code -> lastPromisedReturnCode = code);
        context.respondWith().direct(HttpResponseStatus.OK, "OK");
    }
}
