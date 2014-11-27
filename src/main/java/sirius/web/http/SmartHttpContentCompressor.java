/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.HttpContentCompressor;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpObject;
import io.netty.handler.codec.http.HttpResponse;
import sirius.kernel.commons.Value;

import java.util.List;

/**
 * Better version of {@link HttpContentCompressor} which can be disabled by setting Content-Encoding: Identity for a
 * response.
 * <p>
 * Also it disables itself if the given content is not compressable (jpg, png) or too small (less than 1 kB).
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
class SmartHttpContentCompressor extends HttpContentCompressor {

    private boolean passThrough;

    @Override
    protected void encode(ChannelHandlerContext ctx, HttpObject msg, List<Object> out) throws Exception {
        if (msg instanceof HttpResponse) {
            // Check if this response should be compressed
            HttpResponse res = (HttpResponse) msg;
            // by default compression is on (passThrough bypasses compression)
            passThrough = false;
            // If an "Content-Encoding: Identity" header was set, we do not compress
            if (res.headers().contains(HttpHeaders.Names.CONTENT_ENCODING, HttpHeaders.Values.IDENTITY, false)) {
                passThrough = true;
                // Remove header as one SHOULD NOT send Identity as content encoding.
                res.headers().remove(HttpHeaders.Names.CONTENT_ENCODING);
            } else {
                // If the content type is not compressable (jpg, png ...), we skip compression
                String contentType = res.headers().get(HttpHeaders.Names.CONTENT_TYPE);
                if (!MimeHelper.isCompressable(contentType)) {
                    passThrough = true;
                } else {
                    // If the content length is less than 1 kB but known, we also skip compression
                    int contentLength = Value.of(res.headers().get(HttpHeaders.Names.CONTENT_LENGTH)).asInt(0);
                    if (contentLength > 0 && contentLength < 1024) {
                        passThrough = true;
                    }
                }
            }
        }
        super.encode(ctx, msg, out);
    }

    @Override
    protected Result beginEncode(HttpResponse headers, String acceptEncoding) throws Exception {
        // If compression is skipped, we return null here which disables the compression effectively...
        if (passThrough) {
            return null;
        }
        return super.beginEncode(headers, acceptEncoding);
    }
}
