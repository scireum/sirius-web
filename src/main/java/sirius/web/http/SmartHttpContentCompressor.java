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
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
import io.netty.handler.codec.http.HttpResponse;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Value;

/**
 * Better version of {@link HttpContentCompressor} which can be disabled by setting Content-Encoding: Identity for a
 * response.
 * <p>
 * Also it disables itself if the given content is not compressable (jpg, png) or too small (less than 4 kB).
 */
@SuppressWarnings("squid:MaximumInheritanceDepth")
@Explain("We inherit the hierarchy from netty and cannot change it.")
class SmartHttpContentCompressor extends HttpContentCompressor {

    /**
     * Determines the minimal response length in bytes for compression to be activated.
     * <p>
     * There is no real benefit in compressing really short responses, therefore these are delivered uncompressed.
     */
    public static final int MIN_COMPRESSABLE_CONTENT_LENGTH = 1024;

    @Override
    protected Result beginEncode(HttpResponse res, String acceptEncoding) throws Exception {
        if (!(res instanceof FullHttpResponse)) {
            if (!res.headers().contains(HttpHeaderNames.TRANSFER_ENCODING, HttpHeaderValues.CHUNKED, true)) {
                return null;
            }
        }

        // If the content type is not compressable (jpg, png ...), we skip compression
        String contentType = res.headers().get(HttpHeaderNames.CONTENT_TYPE);
        if (!MimeHelper.isCompressable(contentType)) {
            removeContentLengthForChunkedTransfers(res);
            return null;
        }
        // If the content length is less than 1 kB but known, we also skip compression
        int contentLength = Value.of(res.headers().get(HttpHeaderNames.CONTENT_LENGTH)).asInt(0);
        if (contentLength > 0 && contentLength < MIN_COMPRESSABLE_CONTENT_LENGTH) {
            removeContentLengthForChunkedTransfers(res);
            return null;
        }

        return super.beginEncode(res, acceptEncoding);
    }

    /**
     * Removes the <tt>Content-Length</tt> header from responses having <tt>Transfer-Encoding: chunked</tt>.
     * <p>
     * According to the HTTP standard, a chunked transfer must not provide a content length. We internally still provide
     * this header so that the content compressor (this class) can deciede whether to enable compression or not.
     * However, if the content is left uncompressed, we must remove this header. If it is compressed, the header is
     * removed by the netty implementation as the content length will change anyway.
     *
     * @param res the response to check and remove the header from if it is chunked.
     */
    private void removeContentLengthForChunkedTransfers(HttpResponse res) {
        if (!(res instanceof FullHttpResponse)) {
            // partial responses will always be chunked at this point (see if checks above...)
            res.headers().remove(HttpHeaderNames.CONTENT_LENGTH);
        }
    }
}
