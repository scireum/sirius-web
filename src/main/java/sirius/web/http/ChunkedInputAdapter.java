/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.DefaultHttpContent;
import io.netty.handler.codec.http.HttpContent;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.handler.stream.ChunkedInput;

/**
 * Wrapper class which translates from <code>ChunkedInput&lt;ByteBuf&gt;</code> to <code>ChunkedInput&lt;HttpContent&gt;</code>
 * <p>
 * This is required to make the {@link io.netty.handler.codec.http.HttpContentCompressor} happy as it cannot deal with <code>ByteBuf</code>.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
public class ChunkedInputAdapter implements ChunkedInput<HttpContent> {

    private ChunkedInput<ByteBuf> input;

    /**
     * Creates a new adapter for the given input
     *
     * @param input the chunked input to retrieve chunks from
     */
    public ChunkedInputAdapter(ChunkedInput<ByteBuf> input) {
        this.input = input;
    }

    @Override
    public boolean isEndOfInput() throws Exception {
        return input.isEndOfInput();
    }

    @Override
    public void close() throws Exception {
        input.close();
    }

    @Override
    public HttpContent readChunk(ChannelHandlerContext ctx) throws Exception {
        ByteBuf bb = input.readChunk(ctx);
        if (bb == null || bb.readableBytes() == 0) {
            return LastHttpContent.EMPTY_LAST_CONTENT;
        }
        return new DefaultHttpContent(bb);
    }
}
