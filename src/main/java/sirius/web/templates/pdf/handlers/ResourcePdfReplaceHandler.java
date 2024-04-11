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
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.annotation.Nullable;

/**
 * Resolves resource:// URIs that are referencing {@link Resources} to resized images while maintaining the image
 * ratios.
 */
@Register
public class ResourcePdfReplaceHandler extends PdfReplaceHandler {

    @Part
    private Resources resources;

    @Override
    public boolean accepts(String protocol) {
        return "resource".equals(protocol);
    }

    @Override
    @Nullable
    public FSImage resolveUri(String uri, UserAgentCallback userAgentCallback, int cssWidth, int cssHeight)
            throws Exception {
        String path = Strings.split(uri, "://").getSecond();
        Resource resource = resources.resolve(path).orElse(null);

        if (resource != null) {
            return resizeImage(resolveResource(userAgentCallback, resource.getUrl()), cssWidth, cssHeight);
        }

        return null;
    }
}
