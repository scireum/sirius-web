/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf.handlers;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Image;
import org.xhtmlrenderer.extend.FSImage;
import org.xhtmlrenderer.extend.UserAgentCallback;
import org.xhtmlrenderer.pdf.ITextFSImage;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.AutoRegister;
import sirius.kernel.di.std.Priorized;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.net.URL;

/**
 * Represents a image replace handler that is used by {@link sirius.web.templates.pdf.ImageReplacedElementFactory} to
 * resolve an image URI with a specific protocol.
 */
@AutoRegister
public abstract class PdfReplaceHandler implements Priorized {

    @Override
    public int getPriority() {
        return Priorized.DEFAULT_PRIORITY;
    }

    /**
     * Determines if this handler can resolve a URI with the given protocol.
     *
     * @param protocol the protocol to check
     * @return <tt>true</tt> if {@link #resolveUri(String, UserAgentCallback, int, int)} should be called for the given
     * protocol, <tt>false</tt> otherwise
     */
    public abstract boolean accepts(String protocol);

    /**
     * Resolves the image with the given URI.
     *
     * @param uri               the URI to resolve
     * @param userAgentCallback the user agent to use to resolve image resources
     * @param cssWidth          the requested image width in pixels
     * @param cssHeight         the requested image height in pixels
     * @return the image if the URI could be resolved, <tt>null</tt> otherwise
     * @throws Exception in case of an error while resolving the URI.
     */
    @Nullable
    public abstract FSImage resolveUri(String uri, UserAgentCallback userAgentCallback, int cssWidth, int cssHeight)
            throws Exception;

    /**
     * Resolves the image with the given URL.
     *
     * @param userAgentCallback the user agent to use to resolve image resources
     * @param url               the URL to resolve
     * @return the resolved image
     * @throws BadElementException in case the  resource doesn't have a valid format
     * @throws IOException         in case of an IO error
     */
    protected FSImage resolveResource(UserAgentCallback userAgentCallback, URL url)
            throws BadElementException, IOException {
        return new ITextFSImage(Image.getInstance(url));
    }

    /**
     * Resizes the given image while maintaining the correct ratio.
     *
     * @param image     the image to resize
     * @param cssWidth  the requested image width in pixels
     * @param cssHeight the requested image height in pixels
     * @return the resized image
     */
    protected FSImage resizeImage(FSImage image, int cssWidth, int cssHeight) {
        if (cssWidth != -1 || cssHeight != -1) {
            Tuple<Integer, Integer> newSize = computeResizeBox(cssWidth, cssHeight, image);

            if (newSize != null) {
                image.scale(newSize.getFirst(), newSize.getSecond());
            }
        }

        return image;
    }

    /**
     * Computes a width and height according to cssWidth and cssHeight while maintaining the original aspect ratio.
     *
     * @param cssWidth  the effective width set by attributes or css
     * @param cssHeight the effective height set by attributes or css
     * @param fsImage   the image to scale down
     * @return a new width and height fitting into a rectangle defined by cssWidth/cssHeight
     */
    private Tuple<Integer, Integer> computeResizeBox(int cssWidth, int cssHeight, FSImage fsImage) {
        if (cssWidth == -1 || cssHeight == -1) {
            return null;
        }
        if (fsImage == null) {
            return null;
        }

        if (fsImage.getWidth() > cssWidth || fsImage.getHeight() > cssHeight) {
            return downscaleResource(cssWidth, cssHeight, fsImage.getWidth(), fsImage.getHeight());
        }

        if (cssWidth > fsImage.getWidth() && cssHeight > fsImage.getHeight()) {
            return upscaleResource(cssWidth, cssHeight, fsImage.getWidth(), fsImage.getHeight());
        }

        return null;
    }

    @Nonnull
    private static Tuple<Integer, Integer> downscaleResource(int cssWidth,
                                                             int cssHeight,
                                                             int imageWidth,
                                                             int imageHeight) {

        // First, check if we need to scale down the width
        if (imageWidth > cssWidth) {
            imageHeight = cssWidth * imageHeight / imageWidth;
            imageWidth = cssWidth;
        }

        // Depending on the image aspect ratio, the height might still be larger than the defined limit, so
        // we scale down further
        if (imageHeight > cssHeight) {
            imageWidth = cssHeight * imageWidth / imageHeight;
            imageHeight = cssHeight;
        }

        return Tuple.create(imageWidth, imageHeight);
    }

    @Nonnull
    private static Tuple<Integer, Integer> upscaleResource(int cssWidth,
                                                           int cssHeight,
                                                           int imageWidth,
                                                           int imageHeight) {

        // First, scale up only the width
        imageHeight = cssWidth * imageHeight / imageWidth;
        imageWidth = cssWidth;

        // After upscaling the width, we might need to downscale the height
        return downscaleResource(cssWidth, cssHeight, imageWidth, imageHeight);
    }
}
