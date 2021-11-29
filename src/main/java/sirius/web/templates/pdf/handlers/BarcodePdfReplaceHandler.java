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
import org.xhtmlrenderer.pdf.ITextFSImage;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.web.util.BarcodeController;

import javax.annotation.Nullable;
import java.awt.Color;
import java.awt.Image;

/**
 * Resolves barcode:// URIs to barcode images.
 * <p>
 * The format of the URI needs to match barcode://type/content. As type <b>code128</b>, <b>ean</b>,
 * <b>interleaved2of5</b> and <b>interleaved2of5checksummed</b> are supported. The content is the number that should be
 * represented by the barcode.
 */
@Register
public class BarcodePdfReplaceHandler extends PdfReplaceHandler {

    @Override
    public boolean accepts(String protocol) {
        return "barcode".equals(protocol);
    }

    @Nullable
    @Override
    public FSImage resolveUri(String uri, UserAgentCallback userAgentCallback, int cssWidth, int cssHeight)
            throws Exception {
        String[] barcodeInfo = Strings.split(uri, "://").getSecond().split("/");

        if (barcodeInfo.length != 2) {
            throw new IllegalArgumentException("The URI is required to match the format 'barcode://type/content'");
        }

        Image awtImage = BarcodeController.generateBarcodeImage(barcodeInfo[0], barcodeInfo[1]);

        FSImage fsImage = new ITextFSImage(com.lowagie.text.Image.getInstance(awtImage, Color.WHITE, true));

        if (cssWidth != -1 || cssHeight != -1) {
            fsImage.scale(cssWidth, cssHeight);
        }

        return fsImage;
    }
}
