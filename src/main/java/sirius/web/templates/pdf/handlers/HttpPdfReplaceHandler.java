/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf.handlers;

import org.xhtmlrenderer.extend.FSImage;
import org.xhtmlrenderer.extend.UserAgentCallback;
import sirius.kernel.di.std.Register;

import javax.annotation.Nullable;

/**
 * Resolves http:// and https:// URIs to resized images while maintaining the image ratios.
 */
@Register
public class HttpPdfReplaceHandler extends PdfReplaceHandler {

    @Override
    public boolean accepts(String protocol) {
        return protocol.startsWith("http");
    }

    @Override
    @Nullable
    public FSImage resolveUri(String uri, UserAgentCallback userAgentCallback, int cssWidth, int cssHeight)
            throws Exception {
        return resizeImage(userAgentCallback.getImageResource(uri).getImage(), cssWidth, cssHeight);
    }
}
