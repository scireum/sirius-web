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
import com.lowagie.text.Image;
import com.lowagie.text.pdf.Barcode;
import com.lowagie.text.pdf.Barcode128;
import com.lowagie.text.pdf.BarcodeEAN;
import org.w3c.dom.Element;
import org.xhtmlrenderer.extend.FSImage;
import org.xhtmlrenderer.extend.ReplacedElement;
import org.xhtmlrenderer.extend.ReplacedElementFactory;
import org.xhtmlrenderer.extend.UserAgentCallback;
import org.xhtmlrenderer.layout.LayoutContext;
import org.xhtmlrenderer.pdf.ITextFSImage;
import org.xhtmlrenderer.pdf.ITextImageElement;
import org.xhtmlrenderer.pdf.ITextOutputDevice;
import org.xhtmlrenderer.pdf.ITextReplacedElementFactory;
import org.xhtmlrenderer.render.BlockBox;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;

import java.awt.*;

/**
 * Used by the XHTMLRenderer (creating PDFs) to generate barcodes and to support ratio aware scaling or images.
 * <p>
 * A barcode can be added by placing an img tag with an type attribute: &lt;img type="code128" src="0815" /&gt;.
 * As type <b>code128</b>, <b>ean</b> and <b>qr</b> are supported.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/02
 */
class BarcodeReplacedElementFactory extends ITextReplacedElementFactory implements ReplacedElementFactory {

    /**
     * Generates a new element factory for the given output
     *
     * @param outputDevice the output device to operate on
     */
    BarcodeReplacedElementFactory(ITextOutputDevice outputDevice) {
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
        if (nodeName.equals("img")) {
            if (Strings.isFilled(e.getAttribute("type"))) {
                try {
                    if ("qr".equalsIgnoreCase(e.getAttribute("type"))) {
                        Writer writer = new QRCodeWriter();
                        BitMatrix matrix = writer.encode(e.getAttribute("src"),
                                                         com.google.zxing.BarcodeFormat.QR_CODE,
                                                         cssWidth != -1 ? cssWidth : 600,
                                                         cssHeight != -1 ? cssHeight : 600);
                        FSImage fsImage = new ITextFSImage(Image.getInstance(MatrixToImageWriter.toBufferedImage(matrix),
                                                                             Color.WHITE));
                        if (cssWidth != -1 || cssHeight != -1) {
                            fsImage.scale(cssWidth, cssHeight);
                        }
                        return new ITextImageElement(fsImage);
                    } else {
                        Barcode code = null;
                        if ("code128".equalsIgnoreCase(e.getAttribute("type"))) {
                            code = new Barcode128();
                        } else if ("ean".equalsIgnoreCase(e.getAttribute("type"))) {
                            code = new BarcodeEAN();
                        }
                        code.setCode(e.getAttribute("src"));
                        FSImage fsImage = new ITextFSImage(Image.getInstance(code.createAwtImage(Color.BLACK,
                                                                                                 Color.WHITE),
                                                                             Color.WHITE));
                        if (cssWidth != -1 || cssHeight != -1) {
                            fsImage.scale(cssWidth, cssHeight);
                        }
                        return new ITextImageElement(fsImage);
                    }
                } catch (Throwable e1) {
                    return null;
                }
            } else {
                FSImage fsImage = uac.getImageResource(e.getAttribute("src")).getImage();
                if (fsImage != null) {
                    if (cssWidth != -1 || cssHeight != -1) {
                        Tuple<Integer, Integer> newSize = computeResizeBox(cssWidth, cssHeight, fsImage);
                        if (newSize != null) {
                            fsImage.scale(newSize.getFirst(), newSize.getSecond());
                        }
                    }
                    return new ITextImageElement(fsImage);
                }
            }
        }

        return super.createReplacedElement(c, box, uac, cssWidth, cssHeight);
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

        // Downsize an maintain aspect ratio...
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

        return new Tuple<Integer, Integer>(newWidth, newHeight);
    }
}
