/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf.handlers;

import com.google.zxing.Writer;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;
import com.lowagie.text.Image;
import org.xhtmlrenderer.extend.FSImage;
import org.xhtmlrenderer.extend.UserAgentCallback;
import org.xhtmlrenderer.pdf.ITextFSImage;
import sirius.kernel.commons.Strings;

import javax.annotation.Nullable;
import java.awt.Color;

/**
 * Resolves qr:// URIs to QR code images.
 * <p>
 * The format of the URI needs to match qr://content where content is the data that should be represented by the QR
 * code.
 */
public class QrCodePdfReplaceHandler extends PdfReplaceHandler {

    @Override
    public boolean accepts(String protocol) {
        return "qr".equals(protocol);
    }

    @Nullable
    @Override
    public FSImage resolveUri(String uri, UserAgentCallback userAgentCallback, int cssWidth, int cssHeight)
            throws Exception {
        String data = Strings.split(uri, "://").getSecond();

        Writer writer = new QRCodeWriter();
        BitMatrix matrix = writer.encode(data,
                                         com.google.zxing.BarcodeFormat.QR_CODE,
                                         cssWidth != -1 ? cssWidth : 600,
                                         cssHeight != -1 ? cssHeight : 600);

        FSImage fsImage = new ITextFSImage(Image.getInstance(MatrixToImageWriter.toBufferedImage(matrix), Color.WHITE));

        if (cssWidth != -1 || cssHeight != -1) {
            fsImage.scale(cssWidth, cssHeight);
        }

        return fsImage;
    }
}
