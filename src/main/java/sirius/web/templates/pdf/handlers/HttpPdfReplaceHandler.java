/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf.handlers;

import org.openpdf.text.Utilities;
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
        if (userAgentCallback == null) {
            // built like OpenPDF does, as java.net.URI rejects unencoded characters such as spaces which URLs accept
            return resizeImage(resolveResource(null, Utilities.toURL(uri)), cssWidth, cssHeight);
        }
        return resizeImage(userAgentCallback.getImageResource(uri).getImage(), cssWidth, cssHeight);
    }
}
