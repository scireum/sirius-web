/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.xml.XMLStructuredOutput;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.services.Format;
import sirius.web.services.InternalService;
import sirius.web.resources.Resources;
import sirius.web.services.JSONStructuredOutput;

@Register
public class TestController extends BasicController {

    @Routed("/api/test/test_large_failure")
    @InternalService(format = Format.XML)
    public void test_large_failure(WebContext webContext, XMLStructuredOutput out) throws Exception {
        for (int i = 0; i < 8192; i++) {
            out.property("test" + i, true);
        }
        throw Exceptions.createHandled().withSystemErrorMessage("Expected!").handle();
    }

    @Part
    private Resources resources;

    @Routed("/api/test/test_large")
    @InternalService
    public void test_large(WebContext webContext, JSONStructuredOutput out) throws Exception {
        out.property("test", resources.resolve("assets/test_large.css").get().getContentAsString());
    }

    @Routed("/api/test")
    @InternalService
    public void test(WebContext webContext, JSONStructuredOutput out) throws Exception {
        out.property("test", true);
    }

    @Routed("/api/test/small_large_failure")
    @InternalService
    public void small_large_failure(WebContext webContext, JSONStructuredOutput out) throws Exception {
        out.property("test", true);
        throw Exceptions.createHandled().withSystemErrorMessage("Expected!").handle();
    }

}
