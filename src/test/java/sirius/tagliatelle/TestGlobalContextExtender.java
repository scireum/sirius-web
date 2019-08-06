/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.di.std.Register;
import sirius.web.templates.GlobalContextExtender;

@Register
public class TestGlobalContextExtender implements GlobalContextExtender {

    @Override
    public void collectTemplate(Collector globalParameterCollector) {
        globalParameterCollector.collect("nil", null, Object.class);
    }

    @Override
    public void collectScripting(Collector globalParameterCollector) {
        globalParameterCollector.collect("nothing", null, Object.class);
    }
}
