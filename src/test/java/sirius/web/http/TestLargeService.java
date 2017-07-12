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
import sirius.kernel.xml.StructuredOutput;
import sirius.web.services.ServiceCall;
import sirius.web.services.StructuredService;
import sirius.web.resources.Resources;

@Register(name = "test_large")
public class TestLargeService implements StructuredService {

    @Part
    private Resources resources;

    @Override
    public void call(ServiceCall call, StructuredOutput out) throws Exception {
        out.beginResult();
        out.property("test", resources.resolve("assets/test_large.css").get().getContentAsString());
        out.endResult();
    }
}
