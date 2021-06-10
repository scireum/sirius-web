/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.di.std.Register;
import sirius.web.http.WebContext;

@Register
public class TestController extends BasicController {

    @Routed("/")
    public void index(WebContext webContext) {
        webContext.respondWith().template("/templates/test.html.pasta");
    }
}
