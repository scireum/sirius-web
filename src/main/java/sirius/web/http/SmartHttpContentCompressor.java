/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpContentCompressor;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpResponse;
import sirius.kernel.commons.Value;

/**
 * Better version of {@link HttpContentCompressor} which can be disabled by setting Content-Encoding: Identity for a
 * response.
 * <p>
 * Also it disables itself if the given content is not compressable (jpg, png) or too small (less than 4 kB).
 */
class SmartHttpContentCompressor extends HttpContentCompressor {

    private static final int MIN_COMPRESSABLE_CONTENT_LENGTH = 1024;

    @Override
    protected Result beginEncode(HttpResponse res, String acceptEncoding) throws Exception {
        if (!(res instanceof FullHttpResponse)) {
            if (!HttpHeaders.Values.CHUNKED.equals(res.headers().get(HttpHeaders.Names.TRANSFER_ENCODING))) {
                return null;
            }
        }

        // If the content type is not compressable (jpg, png ...), we skip compression
        String contentType = res.headers().get(HttpHeaders.Names.CONTENT_TYPE);
        if (!MimeHelper.isCompressable(contentType)) {
            return null;
        }
        // If the content length is less than 1 kB but known, we also skip compression
        int contentLength = Value.of(res.headers().get(HttpHeaders.Names.CONTENT_LENGTH)).asInt(0);
        if (contentLength > 0 && contentLength < MIN_COMPRESSABLE_CONTENT_LENGTH) {
            return null;
        }

        return super.beginEncode(res, acceptEncoding);
    }
}
