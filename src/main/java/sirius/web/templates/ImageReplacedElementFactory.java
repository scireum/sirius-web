/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.zxing.Writer;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;
import com.lowagie.text.BadElementException;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.Barcode;
import com.lowagie.text.pdf.Barcode128;
import com.lowagie.text.pdf.BarcodeEAN;
import org.w3c.dom.Element;
import org.xhtmlrenderer.extend.FSImage;
import org.xhtmlrenderer.extend.ReplacedElement;
import org.xhtmlrenderer.extend.UserAgentCallback;
import org.xhtmlrenderer.layout.LayoutContext;
import org.xhtmlrenderer.pdf.ITextFSImage;
import org.xhtmlrenderer.pdf.ITextImageElement;
import org.xhtmlrenderer.pdf.ITextOutputDevice;
import org.xhtmlrenderer.pdf.ITextReplacedElementFactory;
import org.xhtmlrenderer.render.BlockBox;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.security.UserContext;

import javax.annotation.Nonnull;
import java.awt.Color;
import java.io.IOException;

/**
 * Used by the XHTMLRenderer (creating PDFs) to generate barcodes and to support ratio aware scaling of images.
 * <p>
 * A barcode can be added by placing an img tag with an type attribute: &lt;img type="code128" src="0815" /&gt;.
 * As type <b>code128</b>, <b>ean</b> and <b>qr</b> are supported.
 */
class ImageReplacedElementFactory extends ITextReplacedElementFactory {

    private static final String TAG_TYPE_IMG = "img";

    private static final String ATTR_SRC = "src";
    private static final String ATTR_TYPE = "type";

    private static final String PROTOCOL_HTTP = "http";

    private static final String BARCODE_TYPE_QR = "qr";
    private static final String BARCODE_TYPE_CODE128 = "code128";
    private static final String BARCODE_TYPE_EAN = "ean";

    @Part
    private static Resources resources;

    /**
     * Generates a new element factory for the given output
     *
     * @param outputDevice the output device to operate on
     */
    ImageReplacedElementFactory(ITextOutputDevice outputDevice) {
        super(outputDevice);
    }

    @Override
    public ReplacedElement createReplacedElement(LayoutContext c,
                                                 BlockBox box,
                                                 UserAgentCallback uac,
                                                 int cssWidth,
                                                 int cssHeight) {

        Element e = box.getElement();
        if (e == null) {
            return null;
        }

        String nodeName = e.getNodeName();
        if (!TAG_TYPE_IMG.equals(nodeName)) {
            return super.createReplacedElement(c, box, uac, cssWidth, cssHeight);
        }

        String src = e.getAttribute(ATTR_SRC);
        if (Strings.isEmpty(src)) {
            return super.createReplacedElement(c, box, uac, cssWidth, cssHeight);
        }

        try {
            String type = e.getAttribute(ATTR_TYPE);

            if (Strings.isFilled(type)) {
                return createImageForBarcode(type, src, cssWidth, cssHeight);
            }

            ReplacedElement image = createResizedImage(uac, cssWidth, cssHeight, src);

            if (image != null) {
                return image;
            }
        } catch (Exception ex) {
            Exceptions.handle(ex);
        }

        return super.createReplacedElement(c, box, uac, cssWidth, cssHeight);
    }

    private ReplacedElement createResizedImage(UserAgentCallback uac, int cssWidth, int cssHeight, String src)
            throws BadElementException, IOException {
        FSImage image = null;
        if (!src.toLowerCase().startsWith(PROTOCOL_HTTP)) {
            Resource resource = resources.resolve(UserContext.getCurrentScope().getScopeId(), src).orElse(null);
            if (resource != null) {
                try {
                    // First try to load the URL via the user agent - this will somehow result
                    // in better images (correct DPI settings)
                    image = uac.getImageResource(resource.getUrl().toString()).getImage();
                } catch (Exception t) {
                    // Fallback which works but seems to have strange DPI settings sometimes
                    image = new ITextFSImage(Image.getInstance(resource.getUrl()));
                }
            }
        }

        if (image == null) {
            image = uac.getImageResource(src).getImage();
        }

        if (image != null) {
            if (cssWidth != -1 || cssHeight != -1) {
                Tuple<Integer, Integer> newSize = computeResizeBox(cssWidth, cssHeight, image);
                if (newSize != null) {
                    image.scale(newSize.getFirst(), newSize.getSecond());
                }
            }
            return new ITextImageElement(image);
        }
        return null;
    }

    private ReplacedElement createImageForBarcode(String type, String src, int cssWidth, int cssHeight)
            throws Exception {
        if (BARCODE_TYPE_QR.equalsIgnoreCase(type)) {
            Writer writer = new QRCodeWriter();
            BitMatrix matrix = writer.encode(src,
                                             com.google.zxing.BarcodeFormat.QR_CODE,
                                             cssWidth != -1 ? cssWidth : 600,
                                             cssHeight != -1 ? cssHeight : 600);

            FSImage fsImage =
                    new ITextFSImage(Image.getInstance(MatrixToImageWriter.toBufferedImage(matrix), Color.WHITE));

            if (cssWidth != -1 || cssHeight != -1) {
                fsImage.scale(cssWidth, cssHeight);
            }

            return new ITextImageElement(fsImage);
        }

        Barcode code = createBarcode(type);
        code.setCode(src);

        FSImage fsImage =
                new ITextFSImage(Image.getInstance(code.createAwtImage(Color.BLACK, Color.WHITE), Color.WHITE));

        if (cssWidth != -1 || cssHeight != -1) {
            fsImage.scale(cssWidth, cssHeight);
        }

        return new ITextImageElement(fsImage);
    }

    /**
     * Creates an instance of {@link Barcode} that matches the given type descriptor.
     *
     * @param type the requested type
     * @return the barcode
     */
    @Nonnull
    private Barcode createBarcode(String type) {
        if (BARCODE_TYPE_CODE128.equalsIgnoreCase(type)) {
            return new Barcode128();
        }

        if (BARCODE_TYPE_EAN.equalsIgnoreCase(type)) {
            return new BarcodeEAN();
        }

        throw new UnsupportedOperationException("Type is not supported");
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
        if (cssWidth == -1 && cssHeight == -1) {
            return null;
        }

        int newWidth = -1;
        int newHeight = fsImage.getHeight();

        // Downsize and maintain aspect ratio...
        if (fsImage.getWidth() > cssWidth && cssWidth > -1) {
            newWidth = cssWidth;
            newHeight = (newWidth * fsImage.getHeight()) / fsImage.getWidth();
        }

        if (cssHeight > -1 && newHeight > cssHeight) {
            newHeight = cssHeight;
            newWidth = (newHeight * fsImage.getWidth()) / fsImage.getHeight();
        }

        // No resize required
        if (newWidth == -1) {
            return null;
        }

        // No upscaling!
        if (newWidth > fsImage.getWidth() || newHeight > fsImage.getHeight()) {
            return null;
        }

        return Tuple.create(newWidth, newHeight);
    }
}
