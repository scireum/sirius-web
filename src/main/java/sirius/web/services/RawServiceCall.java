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

import java.nio.channels.ClosedChannelException;

/**
 * RAW encoder for calls to a {@link sirius.web.services.StructuredService}.
 *
 * @deprecated the whole StructuredService framework has been deprecated.
 */
@Deprecated
class RawServiceCall extends ServiceCall {

    RawServiceCall(WebContext ctx) {
        super(ctx);
    }

    @Override
    protected void invoke(StructuredService serv) {
        try {
            StructuredOutput output = createOutput();
            serv.call(this, output);
        } catch (ClosedChannelException ex) {
            // If the user unexpectedly closes the connection, we do not need to log an error...
            Exceptions.ignore(ex);
        } catch (Exception t) {
            ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, handle(t));
        }
    }

    @Override
    protected StructuredOutput createOutput() {
        return null;
    }
}
