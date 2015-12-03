/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import sirius.kernel.health.Exceptions;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.http.WebContext;

import java.io.IOException;

/**
 * JSON encoder for calls to a {@link StructuredService}.
 */
class JSONServiceCall extends ServiceCall {

    JSONServiceCall(WebContext ctx) {
        super(ctx);
    }

    @Override
    protected void cleanup(StructuredOutput output) {
        try {
            ((JSONStructuredOutput) output).close();
        } catch (IOException e) {
            Exceptions.handle(ServiceCall.LOG, e);
        }
    }

    @Override
    protected StructuredOutput createOutput() {
        return ctx.respondWith().json();
    }
}
