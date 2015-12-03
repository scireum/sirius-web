/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.health.Exceptions;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.http.WebContext;

/**
 * RAW encoder for calls to a {@link sirius.web.services.StructuredService}.
 */
class RawServiceCall extends ServiceCall {

    RawServiceCall(WebContext ctx) {
        super(ctx);
    }

    @Override
    public void handle(String errorCode, Throwable error) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(error));
    }

    @Override
    protected void cleanup(StructuredOutput output) {

    }

    @Override
    protected StructuredOutput createOutput() {
        return null;
    }
}
