/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.google.common.collect.Lists;
import com.google.common.collect.Queues;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import sirius.kernel.health.Exceptions;

import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Default implementation of {@link ContentHandler} used by {@link sirius.web.controller.ControllerDispatcher}.
 * <p>
 * This handler receives chunks of data which are stored in an internal buffer. This buffer can be accessed via
 * the familiar {@link InputStream} interface. Note that the methods of this implementation might block if either
 * the internal buffer is full, or if no content is currently readable.
 * <p>
 * For stability reasons all blocking methods timeout after a given interval of time leading the handle to be in
 * an <tt>error</tt> state. Therefore all incoming data will be discarded and all read requests will fail as
 * some data might have already been lost anyway.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
public class InputStreamHandler extends InputStream implements ContentHandler {

    /**
     * Defines the default depth (size) of the internal buffer.
     */
    private static final int DEFAULT_BUFFER_DEPTH = 8;

    /**
     * Defines the default read timeout in seconds.
     */
    private static final int DEFAULT_READ_TIMEOUT = 5;

    /**
     * Defines the default write timeout in seconds.
     */
    private static final int DEFAULT_WRITE_TIMEOUT = 15;

    /**
     * Defines the standard time unit used be the default timeout constants.
     */
    private static final TimeUnit DEFAULT_TIME_UNIT = TimeUnit.SECONDS;

    /**
     * Once a read request is issued and no data is available, the calling thread will be blocked until new data
     * comes in. In order to prevent deadlocks or long blocks, the request will timeout (and fail) after the given
     * time in seconds.
     */
    private final int readTimeout;

    /**
     * Once a new data chunk comes in, the calling thread will be blocked until enough space is available in the
     * internal buffer. In order to prevent deadlocks or long blocks, the request will timeout (and fail) after the
     * given time in seconds.
     */
    private final int writeTimeout;

    /**
     * Defines the time unit used by the timeout fields
     */
    private final TimeUnit unit;

    /**
     * Internal buffer which keeps written data until it is read
     */
    private BlockingQueue<ByteBuf> transferQueue;

    /**
     * Contains the chunk on which reads are currently performed
     */
    private ByteBuf currentBuffer;

    /**
     * Contains a flag signalling if the end of the input was already reached
     */
    private volatile boolean eof = false;

    /**
     * Contains a flag indicating if the input stream was closed already
     */
    private volatile boolean open = true;

    /**
     * Contains a flag if a buffering error (timeout) occurred yet. In this case, all incoming data is discarded and
     * all read attempts will fail with an IOException as some data might have been lost already.
     */
    private volatile boolean error = false;

    /**
     * Creates a new handler with default settings for buffer size and timeouts.
     */
    public InputStreamHandler() {
        this(DEFAULT_BUFFER_DEPTH, DEFAULT_READ_TIMEOUT, DEFAULT_WRITE_TIMEOUT, DEFAULT_TIME_UNIT);
    }

    /**
     * Creates a new handler with the given buffer size and timeouts.
     *
     * @param bufferDepth  controls how many chunks are kept in memory before the next call to
     *                     {@link #handle(io.netty.buffer.ByteBuf, boolean)} blocks until some data is read from the
     *                     buffer. Although the number of chunks does not provide an exact measure of the buffer size,
     *                     it should provide a raw estimate as incoming chunks will be between 1024 and 8192 bytes in
     *                     size.
     * @param readTimeout  contains maximal amount of time a reading thread is blocked, waiting for new data,
     *                     before the operation is cancelled by a timeout
     * @param writeTimeout contains maximal amount of time a writing thread is blocked, waiting for buffer space,
     *                     before the operation is cancelled by a timeout
     * @param unit         specifies the time unit used by <tt>readTimeout</tt> and <tt>writeTimeout</tt>
     */
    public InputStreamHandler(int bufferDepth, int readTimeout, int writeTimeout, TimeUnit unit) {
        this.readTimeout = readTimeout;
        this.writeTimeout = writeTimeout;
        this.unit = unit;
        transferQueue = Queues.newArrayBlockingQueue(bufferDepth);
    }

    @Override
    public void handle(ByteBuf content, boolean last) throws IOException {
        try {
            if (eof) {
                // If we already saw a last content block and receive another, this is an illegal state and we abort
                // immediately
                error = true;
                release();
                throw new IOException("Unexpected content after a last chunk as been sent already!");
            }
            if (content.readableBytes() > 0 && !error && open) {
                content.retain();
                if (!transferQueue.offer(content, writeTimeout, unit)) {
                    content.release();
                    error = true;
                    release();
                    throw new IOException("Writing to the buffer queue timed out");
                }
            }
            if (last) {
                // Indicate that no more data can be expected
                eof = true;
                // Offer an empty buffer to unblock any waiting polls...
                transferQueue.offer(Unpooled.EMPTY_BUFFER);
            }
        } catch (InterruptedException e) {
            error = true;
            release();
            throw new IOException("Got interrupted while waiting content to be written", e);
        }
    }

    @Override
    public void cleanup() throws IOException {
        if (open) {
            // Close wasn't called so we consider this stream in an invalid state - this will only affect calls
            // which occur on the reading side. The writing side will now ignore all upcoming writes...
            error = true;
        }
        open = false;
        release();
    }

    @Override
    public void close() throws IOException {
        // Mark stream as closed. Any incoming data will be discarded from now on...
        open = false;
        release();

        super.close();
    }

    private void release() {
        try {
            // Release current buffer if there is one
            if (currentBuffer != null) {
                currentBuffer.release();
                currentBuffer = null;
            }

            // Unlock any waiting poll...
            transferQueue.offer(Unpooled.EMPTY_BUFFER);

            // Drain queue und release all data.
            while (!transferQueue.isEmpty()) {
                List<ByteBuf> unwanted = Lists.newArrayListWithCapacity(transferQueue.size());
                transferQueue.drainTo(unwanted);
                for (ByteBuf buf : unwanted) {
                    buf.release();
                }
            }
        } catch (Throwable t) {
            Exceptions.ignore(t);
        }
    }

    @Override
    public int read() throws IOException {
        ByteBuf buffer = getBuffer();
        if (buffer == null) {
            return -1;
        }
        try {
            return buffer.readByte();
        } finally {
            buffer.release();
        }
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        ByteBuf buffer = getBuffer();
        if (buffer == null) {
            return -1;
        }
        try {
            len = Math.min(buffer.readableBytes(), len);
            buffer.readBytes(b, off, len);
            return len;
        } finally {
            buffer.release();
        }
    }

    @Override
    public long skip(long n) throws IOException {
        ByteBuf buffer = getBuffer();
        if (buffer == null) {
            return -1;
        }
        try {
            if (n > Integer.MAX_VALUE) {
                throw new IllegalArgumentException("n > Integer.MAX_VALUE");
            }
            int nBytes = Math.min(buffer.readableBytes(), (int) n);
            buffer.skipBytes(nBytes);
            return nBytes;
        } finally {
            buffer.release();
        }
    }

    @Override
    public int available() throws IOException {
        ByteBuf buffer = getBuffer();
        if (buffer == null) {
            return 0;
        }
        try {
            return buffer.readableBytes();
        } finally {
            buffer.release();
        }
    }

    @Override
    public synchronized void mark(int readlimit) {
        throw new UnsupportedOperationException();
    }

    @Override
    public synchronized void reset() throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean markSupported() {
        return false;
    }

    /**
     * Determines if this stream is in an ERROR state or not.
     *
     * @return <tt>true</tt> if a failure occurred, <tt>false</tt> otherwise
     */
    public boolean isFailed() {
        return error;
    }

    private ByteBuf getBuffer() throws IOException {
        try {
            if (error) {
                throw new InterruptedIOException("Tried to read from a stream which had an error on either side");
            }
            if (!open) {
                // Stream was closed by reading site...
                error = true;
                release();
                throw new InterruptedIOException("Tried to read an already closed stream");
            }
            if (currentBuffer != null && currentBuffer.refCnt() > 0) {
                if (currentBuffer.readableBytes() > 0) {
                    currentBuffer.retain();
                    return currentBuffer;
                } else {
                    currentBuffer.release();
                    currentBuffer = null;
                }
            }
            if (eof && transferQueue.isEmpty()) {
                return null;
            }
            currentBuffer = transferQueue.poll(readTimeout, unit);
            if (error || currentBuffer == null || currentBuffer.refCnt() == 0) {
                if (currentBuffer != null) {
                    currentBuffer.release();
                    currentBuffer = null;
                }
                //While we were waiting for the net buffer, the input side was closed - signal to reader...
                error = true;
                release();
                throw new IOException(
                        "An error occurred while waiting for upcoming data. Terminating due to possibly inconsistent data!");
            }
            if (currentBuffer.readableBytes() == 0) {
                currentBuffer.release();
                currentBuffer = null;
                return null;
            }
            currentBuffer.retain();
            return currentBuffer;
        } catch (InterruptedException e) {
            error = true;
            release();
            throw new IOException("Got interrupted while waiting for readable content", e);
        }
    }
}
