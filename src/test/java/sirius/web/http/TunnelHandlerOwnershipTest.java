/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.embedded.EmbeddedChannel;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import sirius.kernel.SiriusExtension;

import java.net.InetSocketAddress;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers what a {@link TunnelHandler} owes an upstream connection it stops using.
 * <p>
 * Back-pressure is applied by disabling {@code autoRead} on the upstream channel, but that channel belongs to
 * AsyncHttpClient's connection pool rather than to the request. A single handler can see more than one of them,
 * because a failed attempt is retried and each attempt reports its own channel. Whatever was done to a channel has
 * to be undone before the handler lets go of it - otherwise a pause applied for this request survives on a
 * connection which is about to be handed to somebody else, and that connection can never deliver a response:
 * {@code installBackpressureBridge} is only reached from {@code onHeadersReceived}, and no headers can arrive while
 * reading is disabled.
 * <p>
 * These tests drive the ownership callbacks directly. The full sequence cannot be provoked through a real request:
 * a response small enough for the upstream to finish in one burst disappears into the loopback socket buffers,
 * which makes the consumer writable again and lifts the pause, while a response large enough to keep the consumer
 * blocked causes the upstream to be paused before it has finished - so its connection never reaches the pool.
 */
@ExtendWith(SiriusExtension.class)
class TunnelHandlerOwnershipTest {

    private static final InetSocketAddress ANY_ADDRESS = new InetSocketAddress("localhost", 9999);

    /**
     * Builds a handler which is complete enough to accept the ownership callbacks. None of them touch the client
     * side, so an otherwise empty {@link WebContext} is sufficient.
     */
    private TunnelHandler createHandler() {
        WebContext webContext = new WebContext();
        webContext.setChannelHandlerContext(new EmbeddedChannel().pipeline().firstContext());
        return new TunnelHandler(new Response(webContext), "http://localhost:9999/test", null, null, null);
    }

    @Test
    void pausedConnectionIsRestoredWhenAPooledConnectionReplacesIt() {
        TunnelHandler handler = createHandler();
        EmbeddedChannel firstAttempt = new EmbeddedChannel();
        EmbeddedChannel secondAttempt = new EmbeddedChannel();

        handler.onConnectionPooled(firstAttempt);
        // Stands in for the back-pressure bridge pausing the upstream while this request owned it.
        firstAttempt.config().setAutoRead(false);

        handler.onConnectionPooled(secondAttempt);

        assertTrue(firstAttempt.config().isAutoRead(),
                   "The replaced connection was left unable to read. It is back in the pool in that state, so the"
                   + " next request to receive it cannot be answered until AsyncHttpClient's read timeout fires.");
    }

    @Test
    void pausedConnectionIsRestoredWhenAFreshConnectionReplacesIt() {
        TunnelHandler handler = createHandler();
        EmbeddedChannel firstAttempt = new EmbeddedChannel();
        EmbeddedChannel secondAttempt = new EmbeddedChannel();

        handler.onConnectionPooled(firstAttempt);
        firstAttempt.config().setAutoRead(false);

        // A retry which has to establish a new connection reports it through onTcpConnectSuccess instead.
        handler.onTcpConnectSuccess(ANY_ADDRESS, secondAttempt);

        assertTrue(firstAttempt.config().isAutoRead(),
                   "The replaced connection was left unable to read after a retry established a new one.");
    }
}
