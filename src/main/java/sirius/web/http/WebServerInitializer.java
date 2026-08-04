/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.ChannelException;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.HttpServerCodec;
import io.netty.handler.timeout.IdleStateHandler;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;

import javax.annotation.Nullable;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.TimeUnit;

/**
 * Creates a new pipeline for processing incoming requests of the HTTP web server.
 */
class WebServerInitializer extends ChannelInitializer<SocketChannel> {

    @ConfigValue("http.idleTimeout")
    private Duration idleTimeout;

    @Part
    @Nullable
    private static WebsocketDispatcher websocketDispatcher;

    protected WebServerInitializer() {
    }

    @Override
    public void initChannel(SocketChannel channel) throws Exception {
        enableKeepAlive(channel);

        ChannelPipeline pipeline = channel.pipeline();

        pipeline.addFirst("lowlevel", LowLevelHandler.INSTANCE);
        pipeline.addLast(new HttpServerCodec());
        pipeline.addLast(new HttpPipeliningHandler());
        if (idleTimeout != null && idleTimeout.get(ChronoUnit.SECONDS) > 0) {
            pipeline.addLast("idler",
                             new IdleStateHandler(0, 0, idleTimeout.get(ChronoUnit.SECONDS), TimeUnit.SECONDS));
        }
        pipeline.addLast("compressor", new SmartHttpContentCompressor());
        if (websocketDispatcher != null) {
            pipeline.addLast("websockethandler", new WebsocketHandler(websocketDispatcher));
        }
        pipeline.addLast("handler", new WebServerHandler(isSSL()));
    }

    /**
     * Asks the operating system to send a KEEPALIVE packet every 2h and to expect an ACK on the TCP layer.
     * <p>
     * This is done here rather than as a child option of the bootstrap, because a child option is applied to every
     * accepted socket, including one the client has already reset in the meantime. Setting an option on such a socket
     * fails with <tt>EINVAL</tt> on some platforms (macOS among them), and netty reports that with two warnings and a
     * full stack trace before closing the channel.
     * <p>
     * That is not a rare condition: a client resolving a dual-stack host opens a connection over IPv4 and IPv6 at once
     * and abandons the loser as soon as the other one is established (RFC 8305), which resets it. Every such connect
     * would log a stack trace about a connection that no longer exists and never carried a request. Applying the
     * option here means the failure can be recognised for what it is, while a healthy connection is set up exactly as
     * before.
     *
     * @param channel the accepted channel
     */
    private void enableKeepAlive(SocketChannel channel) {
        try {
            channel.config().setKeepAlive(true);
        } catch (ChannelException exception) {
            Exceptions.ignore(exception);
            WebServer.LOG.FINE("Cannot enable TCP keep-alive for %s, the connection is already gone", channel);
        }
    }

    /**
     * Determines if channels handled via this initializer are protected by TLS (SSL).
     *
     * @return <tt>true</tt> if SSL is present, <tt>false</tt> otherwise
     */
    protected boolean isSSL() {
        return false;
    }
}
