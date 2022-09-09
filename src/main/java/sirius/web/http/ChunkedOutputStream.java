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
import io.netty.channel.ChannelFuture;
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
import java.util.concurrent.TimeUnit;

/**
 * Provides an adapter from {@link OutputStream} to an underlying channel using a buffer.
 */
public class ChunkedOutputStream extends OutputStream {
    private final Response response;
    private final String contentType;
    private final HttpResponseStatus status;
    private volatile boolean open;
    private volatile boolean contentionControl;
    private ByteBuf buffer;

    protected ChunkedOutputStream(Response response, String contentType, HttpResponseStatus status) {
        this.response = response;
        this.contentType = contentType;
        this.status = status;
        open = true;
        buffer = null;
    }

    private void ensureCapacity(int length) {
        if (buffer != null && buffer.writableBytes() < length) {
            flushBuffer(false);
        }
        if (buffer == null) {
            buffer = response.ctx.alloc().buffer(Response.BUFFER_SIZE);
        }
    }

    private void flushBuffer(boolean last) {
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
            try {
                Object message = new DefaultHttpContent(buffer);
                ChannelFuture writeFuture = response.ctx.writeAndFlush(message);
                if (contentionControl) {
                    writeFuture.await(60, TimeUnit.SECONDS);
                }
            } catch (InterruptedException e) {
                Exceptions.handle()
                          .to(WebServer.LOG)
                          .error(e)
                          .withSystemErrorMessage("Got interrupted while waiting for data to be flushed: % (%s)")
                          .handle();

                Thread.currentThread().interrupt();
            }
        }

        buffer = null;
    }

    /**
     * Enables automatic contention control.
     * <p>
     * This essentially blocks writes (once the internal buffer is full) until the data has been flushed out to the
     * client.
     *
     * @return the stream itself for fluent method calls
     */
    public ChunkedOutputStream enableContentionControl() {
        this.contentionControl = true;
        return this;
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
