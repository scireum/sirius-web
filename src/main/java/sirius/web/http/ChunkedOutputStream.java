/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.handler.codec.http.DefaultHttpContent;
import io.netty.handler.codec.http.DefaultLastHttpContent;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpUtil;
import io.netty.handler.codec.http.LastHttpContent;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Provides an adapter from {@link OutputStream} to an underlying channel using a buffer.
 */
class ChunkedOutputStream extends OutputStream {
    private Response response;
    private final String contentType;
    private final HttpResponseStatus status;
    volatile boolean open;
    volatile long bytesWritten;
    ByteBuf buffer;

    ChunkedOutputStream(Response response, String contentType, HttpResponseStatus status) {
        this.response = response;
        this.contentType = contentType;
        this.status = status;
        open = true;
        bytesWritten = 0;
        buffer = null;
    }

    private void ensureCapacity(int length) throws IOException {
        if (buffer != null && buffer.writableBytes() < length) {
            flushBuffer(false);
        }
        if (buffer == null) {
            buffer = response.ctx.alloc().buffer(Response.BUFFER_SIZE);
        }
    }

    private void flushBuffer(boolean last) throws IOException {
        if ((buffer == null || buffer.readableBytes() == 0) && !last) {
            if (buffer != null) {
                buffer.release();
                buffer = null;
            }
            return;
        }

        failIfChannelIsNotOpen();

        if (!response.wc.responseCommitted) {
            createResponse(last);
            if (last) {
                return;
            }
        }

        if (last) {
            completeRequest();
        } else {
            Object message = new DefaultHttpContent(buffer);
            response.contentionAwareWrite(message);
        }

        buffer = null;
    }

    private void completeRequest() {
        if (buffer != null) {
            response.complete(response.ctx.writeAndFlush(new DefaultLastHttpContent(buffer)));
        } else {
            response.complete(response.ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT));
        }
    }

    private void failIfChannelIsNotOpen() {
        if (!response.ctx.channel().isOpen()) {
            open = false;
            if (buffer != null) {
                buffer.release();
                buffer = null;
            }
            throw Exceptions.createHandled().withSystemErrorMessage("Channel was closed").handle();
        }
    }

    private void createResponse(boolean last) {
        if (Strings.isFilled(contentType)) {
            response.addHeaderIfNotExists(HttpHeaderNames.CONTENT_TYPE, contentType);
        }
        response.setDateAndCacheHeaders(System.currentTimeMillis(),
                                        response.cacheSeconds == null || Sirius.isDev() ? 0 : response.cacheSeconds,
                                        response.isPrivate);
        if (response.name != null) {
            response.setContentDisposition(response.name, response.download);
        }
        if (last) {
            ByteBuf initialBuffer = buffer;
            if (initialBuffer == null) {
                initialBuffer = Unpooled.EMPTY_BUFFER;
            }
            HttpResponse res = response.createFullResponse(status, true, initialBuffer);
            HttpUtil.setContentLength(res, initialBuffer.readableBytes());
            response.complete(response.commit(res));
        } else {
            HttpResponse res = response.createChunkedResponse(status, true);
            response.commit(res, false);
        }
    }

    @Override
    public void flush() throws IOException {
        flushBuffer(false);
    }

    @Override
    public void write(int b) throws IOException {
        if (!open) {
            return;
        }
        bytesWritten++;
        ensureCapacity(1);
        buffer.writeByte(b);
    }

    @Override
    public void write(byte[] b) throws IOException {
        write(b, 0, b.length);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        if (!open) {
            return;
        }
        if (len <= 0) {
            return;
        }
        // If the given array is larger than our buffer, we repeatedly call write and limit the length to
        // our buffer size.
        if (len > Response.BUFFER_SIZE) {
            write(b, off, Response.BUFFER_SIZE);
            write(b, off + Response.BUFFER_SIZE, len - Response.BUFFER_SIZE);
            return;
        }
        ensureCapacity(len);
        bytesWritten += len;
        buffer.writeBytes(b, off, len);
    }

    @Override
    public void close() throws IOException {
        if (!open) {
            return;
        }
        open = false;
        super.close();
        if (response.ctx.channel().isOpen()) {
            flushBuffer(true);
        } else if (buffer != null) {
            buffer.release();
            buffer = null;
        }
    }
}
