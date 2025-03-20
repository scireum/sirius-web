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
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.annotation.Nullable;
import java.util.Optional;

/**
 * Resolves resource:// URIs that are referencing {@link Resources} to resized images while maintaining the image
 * ratios.
 */
@Register
public class ResourcePdfReplaceHandler extends PdfReplaceHandler {

    @ConfigValue("product.baseUrl")
    private String productBaseUrl;

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

    @Override
    public Optional<String> tryRewritePlainUrl(String url) {
        if (!url.startsWith(productBaseUrl)) {
            return Optional.empty();
        }

        // remap plain asset URLs to the resource:// scheme
        int indexOfAssets = url.indexOf("/assets/");
        if (indexOfAssets >= 0) {
            return Optional.of("resource:/" + url.substring(indexOfAssets));
        }

        return Optional.empty();
    }
}
