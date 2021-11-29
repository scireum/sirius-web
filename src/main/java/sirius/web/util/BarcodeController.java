/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.Writer;
import com.google.zxing.WriterException;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.datamatrix.DataMatrixWriter;
import com.google.zxing.oned.Code128Writer;
import com.google.zxing.oned.EAN13Writer;
import com.google.zxing.oned.ITFWriter;
import com.google.zxing.qrcode.QRCodeWriter;
import com.lowagie.text.pdf.BarcodeInter25;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Register;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;
import java.util.regex.Pattern;

/**
 * Used to generate barcodes by responding to "/qr" or "/barcode".
 */
@Register
public class BarcodeController extends BasicController {

    private static final String TYPE_QR = "qr";
    private static final String TYPE_DATAMATRIX = "datamatrix";
    private static final String TYPE_CODE128 = "code128";
    private static final String TYPE_EAN = "ean";
    private static final String TYPE_ITF = "itf";
    private static final String TYPE_INTERLEAVED_2_OF_5 = "interleaved2of5";
    private static final String TYPE_INTERLEAVED_2_OF_5_CHECKSUMMED = "interleaved2of5checksummed";

    private static final Pattern NUMERIC = Pattern.compile("[0-9]+");

    /**
     * Creates a QR code for the given content.
     * <p>
     * The parameter <tt>content</tt> determines the contents of the qr code. The parameters <tt>width</tt> and
     * <tt>height</tt> determine its dimensions.
     *
     * @param webContext the current request
     * @throws Exception in case an error occurred when generating the qr code
     */
    @Routed(value = "/qr", priority = 999)
    public void qr(WebContext webContext) throws Exception {
        barcode(webContext, BarcodeFormat.QR_CODE);
    }

    /**
     * Creates a barcode for the given content.
     * <p>
     * The parameter <tt>content</tt> determines the contents of the barcode. The parameters <tt>width</tt> and
     * <tt>height</tt> determine its dimensions.
     *
     * @param webContext the current request
     * @throws Exception in case an error occurred when generating the barcode
     */
    @Routed(value = "/barcode", priority = 999)
    public void barcode(WebContext webContext) throws Exception {
        barcode(webContext, determineFormat(webContext.get("type").asString()));
    }

    private void barcode(WebContext webContext, BarcodeFormat format) throws WriterException, IOException {
        int width = webContext.getFirstFilled("w", "width").asInt(200);
        int height = webContext.getFirstFilled("h", "height").asInt(200);
        String content = webContext.getFirstFilled("c", "content").asString();
        if (Strings.isEmpty(content)) {
            webContext.respondWith()
                      .direct(HttpResponseStatus.BAD_REQUEST, "Usage: /barcode?type=qr&content=...&w=200&h=200");
            return;
        }

        content = alignContentForItfFormat(content, webContext.get("type").asString());

        format = useItfFormatForGtin14(content, format);

        String fileType = webContext.getFirstFilled("fileType").asString("jpg");
        Writer writer = determineWriter(format);
        BitMatrix matrix = writer.encode(content, format, width, height);
        try (OutputStream out = webContext.respondWith()
                                          .infinitelyCached()
                                          .outputStream(HttpResponseStatus.OK,
                                                        MimeHelper.guessMimeType("barcode." + fileType))) {
            MatrixToImageWriter.writeToStream(matrix, fileType, out);
        }
    }

    /**
     * Generates an image of a barcode
     *
     * @param type    the desired barcode type
     * @param content the content of the barcode
     * @return a barcode of the given data as a 200x200 px image
     * @throws WriterException if generating the image fails
     */
    public static Image generateBarcodeImage(String type, String content) throws WriterException {
        if (!NUMERIC.matcher(content).matches()) {
            // contains characters other than digits 0-9 -> directly return a blank image to prevent running into exception
            return new BufferedImage(200, 200, BufferedImage.TYPE_BYTE_GRAY);
        }

        BarcodeFormat format = determineFormat(type);

        content = alignContentForItfFormat(content, type);

        format = useItfFormatForGtin14(content, format);

        Writer writer = determineWriter(format);
        BitMatrix matrix = writer.encode(content, format, 200, 200);
        return MatrixToImageWriter.toBufferedImage(matrix);
    }

    private static BarcodeFormat determineFormat(String format) {
        return switch (Value.of(format).toLowerCase()) {
            case TYPE_QR -> BarcodeFormat.QR_CODE;
            case TYPE_CODE128 -> BarcodeFormat.CODE_128;
            case TYPE_EAN -> BarcodeFormat.EAN_13;
            case TYPE_ITF, TYPE_INTERLEAVED_2_OF_5, TYPE_INTERLEAVED_2_OF_5_CHECKSUMMED -> BarcodeFormat.ITF;
            case TYPE_DATAMATRIX -> BarcodeFormat.DATA_MATRIX;
            default -> throw new IllegalArgumentException(
                    "Unsupported barcode type. Supported types are: qr, code128, ean, interleaved2of5, interleaved2of5checksummed, datamatrix");
        };
    }

    private static Writer determineWriter(BarcodeFormat format) {
        return switch (format) {
            case QR_CODE -> new QRCodeWriter();
            case CODE_128 -> new Code128Writer();
            case EAN_13 -> new EAN13Writer();
            case ITF -> new ITFWriter();
            case DATA_MATRIX -> new DataMatrixWriter();
            default -> throw new IllegalArgumentException("Unsupported barcode type!");
        };
    }

    private static BarcodeFormat useItfFormatForGtin14(String content, BarcodeFormat format) {
        // Adjust the barcode format, if "type=ean" was submitted with the request and a GTIN-14 was given
        if (BarcodeFormat.EAN_13 == format && content.length() == 14) {
            return BarcodeFormat.ITF;
        }

        return format;
    }

    private static String alignContentForItfFormat(String content, String format) {
        if (TYPE_INTERLEAVED_2_OF_5_CHECKSUMMED.equalsIgnoreCase(format)) {
            content += BarcodeInter25.getChecksum(content);
        }

        if ((TYPE_INTERLEAVED_2_OF_5.equalsIgnoreCase(format) || TYPE_INTERLEAVED_2_OF_5_CHECKSUMMED.equalsIgnoreCase(
                format)) && BarcodeInter25.keepNumbers(content).length() % 2 != 0) {
            // Pads the code if the length is uneven
            content = "0" + content;
        }

        return content;
    }
}
