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
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;

import java.io.OutputStream;

/**
 * Used to generate QR codes by responding to /qr
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/12
 */
@Register
public class QRCodeController implements Controller {

    @Override
    public void onError(WebContext ctx, HandledException error) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error);
    }

    @Routed("/qr")
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
