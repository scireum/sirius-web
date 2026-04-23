/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import io.netty.channel.Channel
import io.netty.channel.ChannelHandler
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.ChannelInboundHandlerAdapter
import sirius.kernel.Startable
import sirius.kernel.di.Injector
import java.util.concurrent.ConcurrentHashMap

/**
 * Test-only helper that tracks all server-side child channels accepted by the [WebServer] for the
 * lifetime of the test JVM.
 *
 * The tracker works without any production-code change by attaching a [ChannelHandler] to the
 * front of the listening (server) channel's pipeline. Netty dispatches every accepted child
 * channel through the server channel's pipeline as a [channelRead] event (where `msg` is the new
 * child [Channel]) before `ServerBootstrapAcceptor` takes over. By intercepting that event, the
 * tracker learns about every new connection and registers a close listener to forget the channel
 * when it terminates.
 *
 * Usage:
 * ```
 * TestChannelTracker.install()
 * // ... perform requests ...
 * val pending = TestChannelTracker.totalPendingOutboundBytes()
 * ```
 *
 * [install] is idempotent; the handler is added only on the first call.
 */
internal object TestChannelTracker {

    private const val HANDLER_NAME = "testChannelTracker"

    private val active: MutableSet<Channel> = ConcurrentHashMap.newKeySet()

    /**
     * Installs the tracker on the [WebServer]'s listening channel if it has not been installed
     * yet. Safe to call multiple times.
     */
    fun install() {
        // WebServer is registered for its auto-registered interfaces (Startable, Stoppable, etc.),
        // not for the class itself. Look it up via one of those interfaces.
        val webServer = Injector.context().getParts(Startable::class.java)
            .filterIsInstance<WebServer>()
            .firstOrNull()
            ?: error("WebServer part is not available")
        val serverChannel = extractServerChannel(webServer)
            ?: error("WebServer has not been bound yet - cannot install test channel tracker")

        val pipeline = serverChannel.pipeline()
        if (pipeline.get(HANDLER_NAME) != null) {
            return
        }
        pipeline.addFirst(HANDLER_NAME, Tracker())
    }

    /**
     * Returns a snapshot of the currently tracked child channels.
     */
    fun activeChannels(): Set<Channel> = active.toSet()

    /**
     * Returns the sum of [io.netty.channel.ChannelOutboundBuffer.totalPendingWriteBytes] across
     * all currently tracked channels. Channels without an outbound buffer (e.g. already closed)
     * are ignored.
     */
    fun totalPendingOutboundBytes(): Long = active.sumOf { channel ->
        if (!channel.isOpen) 0L
        else channel.unsafe().outboundBuffer()?.totalPendingWriteBytes() ?: 0L
    }

    private fun extractServerChannel(webServer: WebServer): Channel? {
        val field = WebServer::class.java.getDeclaredField("channel")
        field.isAccessible = true
        return field.get(webServer) as Channel?
    }

    @ChannelHandler.Sharable
    private class Tracker : ChannelInboundHandlerAdapter() {
        override fun channelRead(ctx: ChannelHandlerContext, msg: Any) {
            if (msg is Channel) {
                active.add(msg)
                msg.closeFuture().addListener { active.remove(msg) }
            }
            super.channelRead(ctx, msg)
        }
    }
}
