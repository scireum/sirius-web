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
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;

@Register
public class CompletionPromiseTestController extends BasicController {

    public static int lastPromisedReturnCode = 0;
    public static final Object SIGNAL = new Object();

    @Override
    public void onError(WebContext webContext, HandledException error) {
        webContext.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error);
    }

    @Routed("/test/completion-promise")
    public void completePromise(WebContext context) {
        context.getCompletionPromise().onSuccess(code -> {
            lastPromisedReturnCode = code;
            synchronized (SIGNAL) {
                SIGNAL.notify();
            }
        });
        context.respondWith().direct(HttpResponseStatus.OK, "OK");
    }
}
