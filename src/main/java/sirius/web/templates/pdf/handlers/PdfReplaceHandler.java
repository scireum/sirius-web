/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf.handlers;

import org.apache.batik.anim.dom.SAXSVGDocumentFactory;
import org.apache.batik.util.XMLResourceDescriptor;
import org.openpdf.text.BadElementException;
import org.openpdf.text.Image;
import org.w3c.dom.Element;
import org.w3c.dom.svg.SVGDocument;
import org.xhtmlrenderer.extend.FSImage;
import org.xhtmlrenderer.extend.Size;
import org.xhtmlrenderer.extend.UserAgentCallback;
import org.xhtmlrenderer.pdf.ITextFSImage;
import org.xhtmlrenderer.pdf.SvgImage;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.AutoRegister;
import sirius.kernel.di.std.Priorized;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Locale;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a image replace handler that is used by {@link sirius.web.templates.pdf.ImageReplacedElementFactory} to
 * resolve an image URI with a specific protocol.
 */
@AutoRegister
public abstract class PdfReplaceHandler implements Priorized {

    private static final String SVG_FILE_EXTENSION = ".svg";

    /**
     * Matches an absolute SVG length: a non-negative number with an optional absolute unit.
     */
    private static final Pattern SVG_LENGTH =
            Pattern.compile("\\s*(\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)\\s*(px|pt|pc|mm|cm|in)?\\s*");

    /**
     * Contains the size of SVG images which specify neither width and height nor a view box.
     */
    private static final Size DEFAULT_SVG_SIZE = new Size(300, 150);

    @Override
    public int getPriority() {
        return Priorized.DEFAULT_PRIORITY;
    }

    /**
     * Attempts to rewrite a plain URL to a PDF-compatible one.
     *
     * @param url the plain URL to rewrite
     * @return an optional containing the new URL if the plain URL could be rewritten, an empty optional otherwise
     */
    public Optional<String> tryRewritePlainUrl(String url) {
        return Optional.empty();
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
     * Determines whether errors during URI resolution should be logged.
     * <p>
     * By default, we log all errors. Override this method to change this behavior.
     *
     * @return <tt>true</tt> if errors should be logged, <tt>false</tt> otherwise
     */
    public boolean logErrors() {
        return true;
    }

    /**
     * Resolves the image with the given URL.
     * <p>
     * SVG files (detected by their file extension) are resolved as {@link SvgImage}, which are drawn as vector
     * graphics. All other files are handed to OpenPDF, which only supports raster formats.
     *
     * @param userAgentCallback the user agent to use to resolve image resources
     * @param url               the URL to resolve
     * @return the resolved image
     * @throws BadElementException in case the resource doesn't have a valid format
     * @throws IOException         in case of an IO error
     */
    protected FSImage resolveResource(UserAgentCallback userAgentCallback, URL url)
            throws BadElementException, IOException {
        if (url.getPath().toLowerCase(Locale.ROOT).endsWith(SVG_FILE_EXTENSION)) {
            return resolveSvgResource(url);
        }
        return new ITextFSImage(Image.getInstance(url));
    }

    /**
     * Loads the SVG file with the given URL.
     * <p>
     * The document parsed here is only used to determine the size. When drawn, {@link SvgImage} parses the content
     * again without any document URL, which makes Batik refuse to load external resources the SVG references. This
     * protection is the libraries' and must be kept in mind when changing how SVG images are drawn.
     *
     * @param url the URL of the SVG file
     * @return the loaded SVG image
     * @throws IOException in case of an IO error or if the file isn't a valid SVG document
     */
    private FSImage resolveSvgResource(URL url) throws IOException {
        byte[] content;
        try (InputStream input = url.openStream()) {
            content = input.readAllBytes();
        }

        SAXSVGDocumentFactory factory = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName());
        SVGDocument document = factory.createSVGDocument(null, new ByteArrayInputStream(content));
        return new SvgImage(content, determineSvgSize(document.getDocumentElement()), url.toString());
    }

    /**
     * Determines the intrinsic size of an SVG image in pixels, similar to what flying-saucer does for SVG files it loads
     * itself.
     * <p>
     * Absolute {@code width} and {@code height} attributes win. Otherwise, the size of the {@code viewBox} is used.
     * If neither is present, the default size of replaced elements (300 x 150) applies.
     *
     * @param root the root element of the SVG document
     * @return the intrinsic size, which is at least 1 x 1 so that the image can always be scaled
     */
    private static Size determineSvgSize(Element root) {
        Double width = parseSvgLength(root.getAttribute("width"));
        Double height = parseSvgLength(root.getAttribute("height"));
        if (width != null && height != null) {
            return toSize(width, height);
        }

        String[] viewBox = root.getAttribute("viewBox").trim().split("[\\s,]+");
        if (viewBox.length == 4) {
            Double viewBoxWidth = parseSvgLength(viewBox[2]);
            Double viewBoxHeight = parseSvgLength(viewBox[3]);
            if (viewBoxWidth != null && viewBoxHeight != null) {
                return toSize(viewBoxWidth, viewBoxHeight);
            }
        }

        return DEFAULT_SVG_SIZE;
    }

    /**
     * Parses an absolute SVG length into pixels, assuming 96 dpi like CSS does.
     *
     * @param length the length to parse, with an optional absolute unit
     * @return the length in pixels, or <tt>null</tt> if the length is missing, relative or invalid
     */
    @Nullable
    private static Double parseSvgLength(String length) {
        Matcher matcher = SVG_LENGTH.matcher(length);
        if (!matcher.matches()) {
            return null;
        }

        double value = Double.parseDouble(matcher.group(1));
        return switch (matcher.group(2) == null ? "px" : matcher.group(2)) {
            case "pt" -> value * 96 / 72;
            case "pc" -> value * 16;
            case "mm" -> value * 96 / 25.4;
            case "cm" -> value * 96 / 2.54;
            case "in" -> value * 96;
            default -> value;
        };
    }

    private static Size toSize(double width, double height) {
        return new Size(Math.max(1, (int) Math.round(width)), Math.max(1, (int) Math.round(height)));
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
                return image.scale(newSize.getFirst(), newSize.getSecond());
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
