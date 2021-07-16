/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.xml.StructuredOutput;
import sirius.kernel.xml.XMLStructuredOutput;
import sirius.web.http.WebContext;

import java.nio.charset.StandardCharsets;

/**
 * XML encoder for calls to a {@link StructuredService}.
 *
 * @deprecated the whole StructuredService framework has been deprecated.
 */
@Deprecated(since = "2021/07/01")
class XMLServiceCall extends ServiceCall {

    XMLServiceCall(WebContext ctx) {
        super(ctx);
    }

    @Override
    protected StructuredOutput createOutput() {
        return new XMLStructuredOutput(ctx.respondWith()
                                          .outputStream(HttpResponseStatus.OK,
                                                        "text/xml;charset=" + StandardCharsets.UTF_8.name()));
    }
}
