/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Register;
import sirius.web.http.WebContext;

/**
 * Test controller used to verify controller-wide CSRF validation skipping via {@link Controller#isSkipCsrfValidation()}.
 */
@Register
public class SkipCsrfValidationTestController extends BasicController {

    @Override
    public boolean isSkipCsrfValidation() {
        return true;
    }

    @Routed(value = "/test/controller-skip-csrf", methods = HttpMethod.POST)
    public void controllerSkipCsrf(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed(value = "/test/controller-skip-csrf/explicit-route-skip",
            methods = HttpMethod.POST,
            skipCsrfValidation = true)
    public void controllerAndRouteSkipCsrf(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }
}
