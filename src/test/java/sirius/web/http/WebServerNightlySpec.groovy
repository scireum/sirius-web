/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.alibaba.fastjson2.JSON
import io.netty.bootstrap.Bootstrap
import io.netty.channel.*
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http.*
import org.junit.jupiter.api.Tag
import sirius.kernel.BaseSpecification
import sirius.kernel.Tags


/**
 * Simulates a bunch of "real" (outside) requests through netty and sirius.
 * <p>
 * This ensures a basic performance profile and also makes sure no trivial race conditions or memory leaks
 * are added.
 *
 * This contains long running tests to be exucuted nightly.
 */
@Tag(Tags.NIGHTLY)
class WebServerNightlySpec extends BaseSpecification {

    /**
     * Invoke a web dispatcher which previously blocked the event loop and crashed netty.
     * <p>
     * We now fork a thread for every request so that we never block the event loop
     * in {@link sirius.web.http.Response#contentionAwareWrite(Object, boolean)} but always a worker thread.
     * Therefore the event loop can shovel away the data in the output buffer of the channel
     * and the future will eventually fullfilled.
     */
    def "Invoke /large-blocking-calls with GET"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/large-blocking-calls").openConnection()
        when:
        u.setRequestMethod("GET")
        u.setDoOutput(false)
        and:
        def counter = countBytesInStream(u.getInputStream())
        then:
        180000000 == counter
    }

    private int countBytesInStream(InputStream input) {
        def counter = 0
        def count = 0
        def buffer = new byte[8192]
        while ((count = input.read(buffer)) > 0) {
            counter += count
        }

        return counter
    }

    def "HTTP pipelining is supported correctly"() {
        given:
        List<HttpResponse> responses = new ArrayList<>()
        when:
        EventLoopGroup workerGroup = new NioEventLoopGroup()
        try {
            Bootstrap b = new Bootstrap()
            b.group(workerGroup)
            b.channel(NioSocketChannel.class)
            b.handler(new ChannelInitializer<SocketChannel>() {
                @Override
                void initChannel(SocketChannel ch) throws Exception {
                    ch.pipeline().addLast(new HttpClientCodec())
                    ch.pipeline().addLast(new ChannelInboundHandlerAdapter() {
                        @Override
                        void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
                            if (msg instanceof HttpResponse) {
                                responses.add(msg)
                            }
                            super.channelRead(ctx, msg)
                            if (responses.size() == 3) {
                                ctx.channel().close()
                            }
                        }
                    })
                }
            })

            ChannelFuture f = b.connect("localhost", 9999).sync()
            f.channel().writeAndFlush(new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/pipelining/1000"))
            f.channel().writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
            f.channel().writeAndFlush(new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/pipelining/500"))
            f.channel().writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
            f.channel().writeAndFlush(new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/pipelining/10"))
            f.channel().writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)

            // Wait until the connection is closed.
            f.channel().closeFuture().sync()
        } finally {
            workerGroup.shutdownGracefully()
        }
        then:
        responses.size() == 3
        responses.get(0).headers().get("URI") == "/pipelining/1000"
        responses.get(1).headers().get("URI") == "/pipelining/500"
        responses.get(2).headers().get("URI") == "/pipelining/10"
    }

    def "async JSON calls work"() {
        given:
        def uri = "/test/json/async"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = WebServerSpec.callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == '1'
    }

}
