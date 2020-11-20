/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import com.google.zxing.Writer;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;

import java.io.OutputStream;

/**
 * Used to generate QR codes by responding to /qr
 */
@Register
public class QRCodeController extends BasicController {

    @Override
    public void onError(WebContext webContext, HandledException error) {
        webContext.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error);
    }

    /**
     * Creates an QR code for the given content.
     * <p>
     * The parameter <tt>content</tt> determines the contents of the qr code. The parameters <tt>with</tt> and
     * <tt>height</tt> its dimensions.
     * </p>
     *
     * @param ctx the current request
     * @throws Exception in case an error occures when generatng the qr code
     */
    @Routed(value = "/qr", priority = 999)
    public void qr(WebContext ctx) throws Exception {
        int h = ctx.getFirstFilled("w", "width").asInt(200);
        int w = ctx.getFirstFilled("h", "height").asInt(200);
        String content = ctx.getFirstFilled("c", "content").asString();
        if (Strings.isEmpty(content)) {
            ctx.respondWith().direct(HttpResponseStatus.BAD_REQUEST, "Usage: /qr?conent=...&w=200&h=200");
            return;
        }
        String fileType = ctx.getFirstFilled("jpg").asString("jpg");
        Writer writer = new QRCodeWriter();
        BitMatrix matrix = writer.encode(content, com.google.zxing.BarcodeFormat.QR_CODE, w, h);
        try (OutputStream out = ctx.respondWith()
                                   .outputStream(HttpResponseStatus.OK,
                                                 MimeHelper.guessMimeType("image/" + fileType))) {
            MatrixToImageWriter.writeToStream(matrix, fileType, out);
        }
    }
}
