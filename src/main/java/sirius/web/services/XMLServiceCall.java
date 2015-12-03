/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import com.google.common.base.Charsets;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.xml.StructuredOutput;
import sirius.kernel.xml.XMLStructuredOutput;
import sirius.web.http.WebContext;

/**
 * XML encoder for calls to a {@link StructuredService}.
 */
class XMLServiceCall extends ServiceCall {

    XMLServiceCall(WebContext ctx) {
        super(ctx);
    }

    @Override
    protected StructuredOutput createOutput() {
        return new XMLStructuredOutput(ctx.respondWith()
                                          .outputStream(HttpResponseStatus.OK,
                                                        "text/xml;charset=" + Charsets.UTF_8.name()));
    }
}
