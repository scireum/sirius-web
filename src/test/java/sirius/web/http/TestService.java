/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.di.std.Register;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.services.ServiceCall;
import sirius.web.services.StructuredService;

@Register(name = "test")
public class TestService implements StructuredService {
    @Override
    public void call(ServiceCall call, StructuredOutput out) throws Exception {
        out.beginResult();
        out.property("test", true);
        out.endResult();
    }
}
