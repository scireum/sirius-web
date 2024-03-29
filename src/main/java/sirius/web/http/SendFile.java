/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFuture;
import io.netty.channel.DefaultFileRegion;
import io.netty.handler.codec.http.HttpChunkedInput;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.handler.ssl.SslHandler;
import io.netty.handler.stream.ChunkedFile;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.health.Exceptions;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Sends a file as static content.
 */
class SendFile {

    private static final Pattern RANGE_HEADER = Pattern.compile("bytes=(\\d+)?-(\\d+)?");

    private final Response response;
    private File file;
    private RandomAccessFile randomAccessFile;
    private String contentType;
    private long contentStart;
    private long expectedContentLength;
    private Tuple<Long, Long> range;

    SendFile(Response response) {
        this.response = response;
    }

    void send(File fileToSend) {
        try {
            this.file = fileToSend;
            if (file.isHidden() || !file.exists() || !file.isFile()) {
                response.error(HttpResponseStatus.NOT_FOUND);
                return;
            }
            determineContentType();

            if (response.handleIfModifiedSince(file.lastModified())) {
                return;
            }

            randomAccessFile = new RandomAccessFile(file, "r");
            response.setDateAndCacheHeaders(file.lastModified(),
                                            response.cacheSeconds == null ?
                                            Response.obtainClientDurationInSeconds() :
                                            response.cacheSeconds,
                                            response.isPrivate);

            if (!parseRangesAndUpdateHeaders()) {
                response.error(HttpResponseStatus.REQUESTED_RANGE_NOT_SATISFIABLE, "Cannot parse 'accept-ranges'.");
                return;
            }

            if (response.name != null) {
                response.setContentDisposition(response.name, response.download);
            }
            sendFileResponse();
        } catch (Exception exception) {
            response.internalServerError("File: " + file.getAbsolutePath(), exception);
        }
    }

    private boolean parseRangesAndUpdateHeaders() throws IOException {
        try {
            response.addHeaderIfNotExists(HttpHeaderNames.ACCEPT_RANGES, HttpHeaderValues.BYTES);
            contentStart = 0;
            expectedContentLength = randomAccessFile.length();
            range = parseRange(randomAccessFile.length());
            if (range == null) {
                response.addHeaderIfNotExists(HttpHeaderNames.CONTENT_LENGTH, expectedContentLength);
            } else {
                contentStart = range.getFirst();
                expectedContentLength = range.getSecond() - range.getFirst() + 1;
                response.setHeader(HttpHeaderNames.CONTENT_LENGTH, expectedContentLength);
                response.setHeader(HttpHeaderNames.CONTENT_RANGE,
                                   "bytes "
                                   + range.getFirst()
                                   + "-"
                                   + range.getSecond()
                                   + "/"
                                   + randomAccessFile.length());
            }

            return true;
        } catch (IllegalArgumentException ignored) {
            Exceptions.ignore(ignored);
            return false;
        }
    }

    private void determineContentType() {
        contentType = MimeHelper.guessMimeType(response.name != null ? response.name : file.getName());
        response.addHeaderIfNotExists(HttpHeaderNames.CONTENT_TYPE, contentType);
    }

    /*
     * Determines if we're running on SSL
     */
    private boolean isSSL() {
        return response.getChannelHandlerContext().channel().pipeline().get(SslHandler.class) != null;
    }

    private boolean sendFileResponse() throws IOException {
        HttpResponseStatus responseStatus = range != null ? HttpResponseStatus.PARTIAL_CONTENT : HttpResponseStatus.OK;
        HttpResponse res;
        if (response.canBeCompressed(contentType)
            && expectedContentLength > SmartHttpContentCompressor.MIN_COMPRESSABLE_CONTENT_LENGTH) {
            res = response.createChunkedResponse(responseStatus, true);
        } else {
            res = response.createResponse(responseStatus, true);
        }
        response.commit(res, false);
        response.installChunkedWriteHandler();
        ChannelFuture writeFuture = executeChunkedWrite();
        writeFuture.addListener(ignored -> randomAccessFile.close());
        response.removedChunkedWriteHandler(writeFuture);

        response.complete(writeFuture);
        return false;
    }

    private ChannelFuture executeChunkedWrite() throws IOException {
        if (response.responseChunked) {
            // Send chunks of data which can be compressed
            response.getChannelHandlerContext()
                    .write(new HttpChunkedInput(new ChunkedFile(randomAccessFile,
                                                                contentStart,
                                                                expectedContentLength,
                                                                Response.BUFFER_SIZE)));
            return response.getChannelHandlerContext().writeAndFlush(Unpooled.EMPTY_BUFFER);
        } else if (isSSL()) {
            response.getChannelHandlerContext()
                    .write(new ChunkedFile(randomAccessFile,
                                           contentStart,
                                           expectedContentLength,
                                           Response.BUFFER_SIZE));
            return response.getChannelHandlerContext().writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
        } else {
            // Send file using zero copy approach!
            response.getChannelHandlerContext()
                    .write(new DefaultFileRegion(randomAccessFile.getChannel(), contentStart, expectedContentLength));
            return response.getChannelHandlerContext().writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
        }
    }

    private Tuple<Long, Long> parseRange(long availableLength) {
        String header = response.getWebContext().getHeader(HttpHeaderNames.RANGE);
        if (Strings.isEmpty(header)) {
            return null;
        }
        Matcher m = RANGE_HEADER.matcher(header);
        if (!m.matches()) {
            throw new IllegalArgumentException(Strings.apply("Range does not match the expected format: %s", header));
        }
        Tuple<Long, Long> result = Tuple.create();
        if (Strings.isFilled(m.group(1))) {
            result.setFirst(Long.parseLong(m.group(1)));
        } else {
            result.setFirst(availableLength - Long.parseLong(m.group(2)));
            result.setSecond(availableLength - 1);
            return result;
        }
        result.setFirst(Long.parseLong(m.group(1)));
        if (Strings.isFilled(m.group(2))) {
            result.setSecond(Long.parseLong(m.group(2)));
        } else {
            result.setSecond(availableLength - 1);
        }
        if (result.getSecond() < result.getFirst()) {
            return null;
        }
        if (result.getSecond() >= availableLength) {
            throw new IllegalArgumentException(Strings.apply("End of range is beyond the end of available data: %s",
                                                             header));
        }

        return result;
    }
}
