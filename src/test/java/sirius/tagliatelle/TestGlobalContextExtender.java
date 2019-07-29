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

import java.util.function.BiConsumer;

@Register
public class TestGlobalContextExtender implements GlobalContextExtender {

    @Override
    public void collectTemplate(BiConsumer<String, Object> globalParameterCollector) {
        globalParameterCollector.accept("nil", null);
    }

    @Override
    public void collectScripting(BiConsumer<String, Object> globalParameterCollector) {
        globalParameterCollector.accept("nothing", null);
    }
}
