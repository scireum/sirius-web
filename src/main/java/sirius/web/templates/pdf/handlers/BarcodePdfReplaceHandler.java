/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf.handlers;

import com.google.zxing.WriterException;
import com.lowagie.text.pdf.Barcode;
import com.lowagie.text.pdf.BarcodeInter25;
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

    private static final String BARCODE_TYPE_CODE128 = "code128";
    private static final String BARCODE_TYPE_EAN = "ean";
    private static final String BARCODE_TYPE_INTERLEAVED_2_OF_5 = "interleaved2of5";
    private static final String BARCODE_TYPE_INTERLEAVED_2_OF_5_CHECKSUMMED = "interleaved2of5checksummed";

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

        Image awtImage = getBarcodeImage(barcodeInfo[0], barcodeInfo[1], cssWidth, cssHeight);

        int scaleFactor = calculateBarcodeScaleFactor(cssWidth, cssHeight, awtImage);

        awtImage = awtImage.getScaledInstance(awtImage.getWidth(null) * scaleFactor,
                                              awtImage.getHeight(null) * scaleFactor,
                                              Image.SCALE_REPLICATE);

        FSImage fsImage = new ITextFSImage(com.lowagie.text.Image.getInstance(awtImage, Color.WHITE, true));

        if (cssWidth != -1 || cssHeight != -1) {
            fsImage.scale(cssWidth, cssHeight);
        }

        return fsImage;
    }

    private Image getBarcodeImage(String barcodeType, String content, int width, int height) throws WriterException {
        assertSupportedBarcodeType(barcodeType);

        if (BARCODE_TYPE_INTERLEAVED_2_OF_5_CHECKSUMMED.equalsIgnoreCase(barcodeType)) {
            Barcode code = new BarcodeInter25();
            code.setGenerateChecksum(true);
            code.setCode(padCodeIfNecessary(code, content));
            return code.createAwtImage(Color.BLACK, Color.WHITE);
        }

        return BarcodeController.getBarcodeImage(barcodeType, content, width, height);
    }

    private int calculateBarcodeScaleFactor(int cssWidth, int cssHeight, Image awtImage) {
        return (int) Math.max(Math.ceil(cssWidth / (float) awtImage.getWidth(null)),
                              Math.ceil(cssHeight / (float) awtImage.getHeight(null)));
    }

    private void assertSupportedBarcodeType(String type) {
        if (!BARCODE_TYPE_CODE128.equalsIgnoreCase(type)
            && !BARCODE_TYPE_EAN.equalsIgnoreCase(type)
            && !BARCODE_TYPE_INTERLEAVED_2_OF_5.equalsIgnoreCase(type)
            && !BARCODE_TYPE_INTERLEAVED_2_OF_5_CHECKSUMMED.equalsIgnoreCase(type)) {
            throw new UnsupportedOperationException(Strings.apply(
                    "Type '%s' is not supported. Supported types are: code128, ean, interleaved2of5, interleaved2of5checksummed.",
                    type));
        }
    }

    /**
     * Pads the code if necessary.
     * <p>
     * Unfortunately padding will not be added automatically when using <b>interleaved2of5</b> or
     * <b>interleaved2of5checksummed</b>. Thus we manually prepend a zero if the code length is uneven.
     *
     * @param code the instance of the barcode
     * @param src  the code from the src attribute
     * @return the padded code, or the original code if padding was not needed
     */
    private String padCodeIfNecessary(Barcode code, String src) {
        if (code instanceof BarcodeInter25) {
            int length = BarcodeInter25.keepNumbers(src).length();

            // Length is uneven and no checksum will be added
            if (length % 2 != 0 && !code.isGenerateChecksum()) {
                return "0" + src;
            }

            // Length is even but a checksum will be added
            if (length % 2 == 0 && code.isGenerateChecksum()) {
                return "0" + src;
            }
        }

        return src;
    }
}
