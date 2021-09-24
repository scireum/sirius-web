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
import com.google.zxing.qrcode.QRCodeWriter;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Used to generate barcodes by responding to "/qr" or "/barcode".
 */
@Register
public class BarcodeController extends BasicController {

    /**
     * Creates an QR code for the given content.
     * <p>
     * The parameter <tt>content</tt> determines the contents of the qr code. The parameters <tt>with</tt> and
     * <tt>height</tt> its dimensions.
     * </p>
     *
     * @param ctx the current request
     * @throws Exception in case an error occurred when generating the qr code
     */
    @Routed(value = "/qr", priority = 999)
    public void qr(WebContext ctx) throws Exception {
        barcode(ctx, BarcodeFormat.QR_CODE);
    }

    @Routed(value = "/barcode", priority = 999)
    public void barcode(WebContext webContext) throws Exception {
        barcode(webContext, determineFormat(webContext.get("type").asString()));
    }

    private void barcode(WebContext webContext, BarcodeFormat format) throws WriterException, IOException {
        int width = webContext.getFirstFilled("w", "width").asInt(200);
        int height = webContext.getFirstFilled("h", "height").asInt(200);
        String content = webContext.getFirstFilled("c", "content").asString();
        if (Strings.isEmpty(content)) {
            webContext.respondWith().direct(HttpResponseStatus.BAD_REQUEST, "Usage: /barcode?type=qr&content=...&w=200&h=200");
            return;
        }

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

    private BarcodeFormat determineFormat(String format) {
        return switch (format) {
            case "qr" -> BarcodeFormat.QR_CODE;
            case "code128" -> BarcodeFormat.CODE_128;
            case "ean" -> BarcodeFormat.EAN_13;
            case "datamatrix" -> BarcodeFormat.DATA_MATRIX;
            default -> throw new IllegalArgumentException(
                    "Unsupported barcode type. Supported types are: qr, code128, ean, datamatrix");
        };
    }

    private Writer determineWriter(BarcodeFormat format) {
        return switch (format) {
            case QR_CODE -> new QRCodeWriter();
            case CODE_128 -> new Code128Writer();
            case EAN_13 -> new EAN13Writer();
            case DATA_MATRIX -> new DataMatrixWriter();
            default -> throw new IllegalArgumentException("Unsupported barcode type!");
        };
    }
}