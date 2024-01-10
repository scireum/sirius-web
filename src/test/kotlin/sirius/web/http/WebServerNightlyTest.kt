/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import io.netty.bootstrap.Bootstrap
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.ChannelInboundHandlerAdapter
import io.netty.channel.ChannelInitializer
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http.*
import org.junit.jupiter.api.Tag
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.Tags
import sirius.kernel.commons.Json
import java.io.InputStream
import java.net.HttpURLConnection
import java.net.URI
import kotlin.test.assertEquals

/**
 * Simulates a bunch of "real" (outside) requests through netty and sirius.
 * <p>
 * This ensures a basic performance profile and also makes sure no trivial race conditions or memory leaks
 * are added.
 *
 * This contains long running tests to be executed nightly.
 */
@Tag(Tags.NIGHTLY)
@ExtendWith(SiriusExtension::class)
class WebServerNightlyTest {

    /**
     * Invoke a web dispatcher which previously blocked the event loop and crashed netty.
     * <p>
     * We now fork a thread for every request so that we never block the event loop
     * in {@link sirius.web.http.Response#contentionAwareWrite(Object, boolean)} but always a worker thread.
     * Therefore the event loop can shovel away the data in the output buffer of the channel
     * and the future will eventually fullfilled.
     */
    @Test
    fun `Invoke large-blocking-calls with GET`() {

        val url = URI("http://localhost:9999/large-blocking-calls").toURL().openConnection() as HttpURLConnection

        url.setRequestMethod("GET")
        url.setDoOutput(false)

        val counter = countBytesInStream(url.inputStream)

        assertEquals(180000000, counter)
    }

    private fun countBytesInStream(input: InputStream): Int {
        var counter = 0
        var count = 0
        val buffer = ByteArray(8192)
        while ((input.read(buffer).also { count = it }) > 0) {
            counter += count
        }

        return counter
    }

    @Test
    fun `HTTP pipelining is supported correctly`() {

        val responses = ArrayList<HttpResponse>()

        val workerGroup = NioEventLoopGroup()

        try {
            val b = Bootstrap()
            b.group(workerGroup)
            b.channel(NioSocketChannel::class.java)
            b.handler(object : ChannelInitializer<SocketChannel>() {
                override fun initChannel(ch: SocketChannel) {
                    ch.pipeline().addLast(HttpClientCodec())
                    ch.pipeline().addLast(object : ChannelInboundHandlerAdapter() {
                        override fun channelRead(ctx: ChannelHandlerContext, msg: Any) {
                            if (msg is HttpResponse) {
                                responses.add(msg)
                            }
                            super.channelRead(ctx, msg)
                            if (responses.size == 3) {
                                ctx.channel().close()
                            }
                        }
                    })
                }
            })

            val f = b.connect("localhost", 9999).sync()
            f.channel().writeAndFlush(DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/pipelining/1000"))
            f.channel().writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
            f.channel().writeAndFlush(DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/pipelining/500"))
            f.channel().writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
            f.channel().writeAndFlush(DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/pipelining/10"))
            f.channel().writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)

            // Wait until the connection is closed.
            f.channel().closeFuture().sync()

        } finally {
            workerGroup.shutdownGracefully()
        }

        assertEquals(3, responses.size)
        assertEquals("/pipelining/1000", responses[0].headers().get("URI"))
        assertEquals("/pipelining/500", responses[1].headers().get("URI"))
        assertEquals("/pipelining/10", responses[2].headers().get("URI"))
    }

    @Test
    fun `async JSON calls work`() {

        val uri = "/test/json/async"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = WebServerSpec.callAndRead(uri, null, expectedHeaders)

        assertEquals("1", Json.parseObject(data).get("test").asText())
    }

}
