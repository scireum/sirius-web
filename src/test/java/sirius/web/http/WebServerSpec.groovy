/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.alibaba.fastjson.JSON
import com.google.common.base.Charsets
import com.google.common.collect.Lists
import com.google.common.io.ByteStreams
import io.netty.bootstrap.Bootstrap
import io.netty.channel.*
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http.*
import org.apache.log4j.Level
import sirius.kernel.BaseSpecification
import sirius.kernel.commons.Strings
import sirius.kernel.health.LogHelper

/**
 * Simulates a bunch of "real" (outside) requests through netty and sirius.
 * <p>
 * This ensures a basic performance profile and also makes sure no trivial race conditions or memory leaks
 * are added.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2015/05
 */
class WebServerSpec extends BaseSpecification {

    def callAndRead(String uri, Map outHeaders, Map expectedHeaders) {
        URLConnection c = new URL("http://localhost:9999" + uri).openConnection()
        outHeaders.each { k, v -> c.addRequestProperty(k, v) }
        c.connect()
        def result = new String(ByteStreams.toByteArray(c.getInputStream()), Charsets.UTF_8)
        expectedHeaders.each { k, v ->
            if (!Strings.areEqual(c.getHeaderField(k), v)) {
                throw new IllegalStateException("Header: " + k + " was " + c.getHeaderField(k) + " instead of " + v)
            }
        }

        return result
    }

    def "Invoke /assets/test.css to test"() {
        given:
        def uri = "/assets/test.css"
        def headers = ['accept-encoding': 'gzip']
        // File is too small to be compressed!
        def expectedHeaders = ['content-encoding': null]
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        "body { background-color: #000000; }" == data
    }

    def "Invoke /assets/test_large.css"() {
        given:
        def uri = "/assets/test_large.css"
        def expectedHeaders = ['content-encoding': null]
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        60314 == data.length()
    }

    def "Invoke /assets/test_large.css with GZIP"() {
        given:
        def uri = "/assets/test_large.css"
        def headers = ['accept-encoding': 'gzip']
        def expectedHeaders = ['content-encoding': 'gzip']
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        // URLConnection does not understand GZIP and therefore does not unzip... :-(
        1298 == data.length()
    }

    def "Invoke /test/resource to access a resource"() {
        given:
        def uri = "/test/resource"
        def headers = ['accept-encoding': 'gzip']
        def expectedHeaders = ['content-encoding': 'gzip']
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        // URLConnection does not understand GZIP and therefore does not unzip... :-(
        1298 == data.length()
    }

    def "Invoke /test/resource_uncompressable to access a non-compressable resource"() {
        given:
        def uri = "/test/resource_uncompressable"
        def headers = ['accept-encoding': 'gzip']
        def expectedHeaders = ['content-encoding': null]
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        60_314 == data.length()
    }

    /**
     * Determines if redispatching in the TestDispatcher works.
     */
    def "Redispatching works"() {
        given:
        def uri = "/redispatch"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"test":true}' == data
    }

    /**
     * Call a small service which result fits into a single response chunk...
     */
    def "Invoke /service/json/test"() {
        given:
        def uri = "/service/json/test"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"test":true}' == data
    }

    /**
     * Call a large service to test buffer-based output streams
     */
    def "Invoke /service/json/test_large"() {
        given:
        def uri = "/service/json/test_large"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        // Size should be contents of large test file plus json overhead and escaping....
        60543 == data.length()
    }

    /**
     * Call a service which generates a small result and then fails.
     * <p>
     * We expect an appropriate error in this case.
     */
    def "Invoke /service/xml/test_large_failure and expect a proper error"() {
        given:
        def uri = "/service/xml/test_large_failure"
        def expectedHeaders = ['content-type': 'text/xml;charset=UTF-8']
        when:
        LogHelper.clearMessages()
        and:
        def data = callAndRead(uri, null, expectedHeaders)
        then: "We expect a warning as the server was unable to send an error"
        LogHelper.hasMessage(Level.WARN, "services", "Cannot send service error for.*")
        and: "As the connection is closed due to an inconsistent state, an IO exception will occur on the client side"
        thrown(IOException)
    }

    /**
     * Call a controller which tunnels a small file
     */
    def "Invoke /tunnel/test"() {
        given:
        def uri = "/tunnel/test"
        def expectedHeaders = ['content-type': 'text/test']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"test":true}' == data
    }

    /**
     * Call a controller which tunnels a large file
     */
    def "Invoke /tunnel/test_large"() {
        given:
        def uri = "/tunnel/test_large"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        // Size should be contents of large test file plus json overhead and escaping....
        60543 == data.length()
    }

    /**
     * Call a controller which uses a fallback for a failed tunnel (404)
     */
    def "Invoke /tunnel/fallback_for_404 and expect the fallback to work"() {
        given:
        def uri = "/tunnel/fallback_for_404"
        def expectedHeaders = ['content-type': 'text/test']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"test":true}' == data
    }

    /**
     * Call a controller which uses a fallback for a failed tunnel (connection error)
     */
    def "Invoke /tunnel/fallback_for_error and expect the fallback to work"() {
        given:
        def uri = "/tunnel/fallback_for_error"
        def expectedHeaders = ['content-type': 'text/test']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"test":true}' == data
    }

    /**
     * Call a controller which uses JSON Calls
     */
    def "Invoke /test/json testing built in JSON handling"() {
        given:
        def uri = "/test/json?test=Hello_World"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == 'Hello_World'
    }

    def "Invoke /test/json-param testing built in JSON handling"() {
        given:
        def uri = "/test/json-param/Hello"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == 'Hello'
    }

    def "Invoke /test/json-params/1/2 testing multiple parameter"() {
        given:
        def uri = "/test/json-params/1/2"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("param1") == '1'
        and:
        JSON.parseObject(data).get("param2") == '2'
    }

    def "Invoke /test/mixed-json-params/2/1 testing mixed parameter order"() {
        given:
        def uri = "/test/mixed-json-params/2/1"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("param1") == '1'
        and:
        JSON.parseObject(data).get("param2") == '2'
    }

    def "Invoke /test/json-params-varargs/1/2/3/4/5/6/7/8/9 testing varargs"() {
        given:
        def uri = "/test/json-params-varargs/1/2/3/4/5/6/7/8/9"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("param1") == '1'
        and:
        JSON.parseObject(data).get("param2") == '2'
        and:
        def varargs = JSON.parseObject(data).getJSONArray("params")
        varargs.size() == 7
        varargs.get(0) == '3'
        varargs.get(6) == '9'
    }

    /**
     * Call a controller which uses predispatching
     */
    def "Invoke /test/presidpatch with POST"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/predispatch").openConnection()
        and:
        def testByteArray = "Hello Service".getBytes()
        when:
        u.setRequestMethod("POST")
        u.setDoInput(true)
        u.setDoOutput(true)
        def out = u.getOutputStream()
        for (int i = 0; i < 1024; i++) {
            out.write(testByteArray)
        }
        out.close()
        def result = new String(ByteStreams.toByteArray(u.getInputStream()), Charsets.UTF_8)
        then:
        String.valueOf(testByteArray.length * 1024) == result
    }

    /**
     * Call a controller which uses POST
     */
    def "Invoke /test/post with POST"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/post").openConnection()
        and:
        def testString = "value=Hello"
        when:
        u.setRequestMethod("POST")
        u.setRequestProperty("Content-Type",
                "application/x-www-form-urlencoded")

        u.setRequestProperty("Content-Length", Integer.toString(testString.getBytes().length))
        u.setDoInput(true)
        u.setDoOutput(true)
        def out = u.getOutputStream()
        out.write(testString.getBytes(Charsets.UTF_8))
        out.close()
        def result = new String(ByteStreams.toByteArray(u.getInputStream()), Charsets.UTF_8)
        then:
        "Hello" == result
    }

    def "test that outputstreams work"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/os").openConnection()
        when:
        u.setRequestMethod("GET")
        u.setDoInput(true)
        u.setDoOutput(false)
        def arr = ByteStreams.toByteArray(u.getInputStream())
        then:
        9 * 8192 == arr.length

    }

    /**
     * Test an empty POST
     */
    def "Invoke /test/post with empty POST"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/post").openConnection()
        and:
        def testString = ""
        when:
        u.setRequestMethod("POST")
        u.setRequestProperty("Content-Type",
                "application/x-www-form-urlencoded")

        u.setRequestProperty("Content-Length", Integer.toString(testString.getBytes().length))
        u.setDoInput(true)
        u.setDoOutput(true)
        def out = u.getOutputStream()
        out.write(testString.getBytes(Charsets.UTF_8))
        out.close()
        def result = new String(ByteStreams.toByteArray(u.getInputStream()), Charsets.UTF_8)
        then:
        "" == result
    }

    /**
     * Ensure that predispatching does not trigger on GET requests
     */
    def "Invoke /test/presidpatch with GET"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/predispatch").openConnection()
        when:
        u.setRequestMethod("GET")
        then:
        u.getResponseCode() == 404
    }

    def "HTTP pipelining is supported correctly"() {
        given:
        List<HttpResponse> responses = Lists.newArrayList()
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
    /**
     * Test correct decoding
     */
    def "Invoke /test/json testing correct decoding delimiter"() {
        given:
        def uri = "/test/json?test=Hello%2FWorld"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == 'Hello/World'
    }

    def "Invoke /test/json testing correct decoding space"() {
        given:
        def uri = "/test/json?test=Hello%20World"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == 'Hello World'
    }

    def "Invoke /test/json-param testing correct decoding delimiter"() {
        given:
        def uri = "/test/json-param/Hello%2FWorld"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == 'Hello/World'
    }

    def "Invoke /test/json-param testing correct decoding space"() {
        given:
        def uri = "/test/json-param/Hello%20World"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == 'Hello World'
    }

    def "Invoke /test/json-params/one/t%2Fwotesting multiple parameter decoding delimiter"() {
        given:
        def uri = "/test/json-params/one/t%2Fwo"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("param1") == 'one'
        and:
        JSON.parseObject(data).get("param2") == 't/wo'
    }

    def "Invoke /test/json-params/one/t%20wo testing multiple parameter decoding space"() {
        given:
        def uri = "/test/json-params/one/t%20wo"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("param1") == 'one'
        and:
        JSON.parseObject(data).get("param2") == 't wo'
    }

    def "Invoke /test/json-params-varargs/1%2F/%2F2/one/t%2Fwo/t%2Fhree/%2Ffour/five%2F testing varargs decoding delimiter"() {
        given:
        def uri = "/test/json-params-varargs/1%2F/%2F2/one/t%2Fwo/t%2Fhree/%2Ffour/five%2F"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("param1") == '1/'
        and:
        JSON.parseObject(data).get("param2") == '/2'
        and:
        def varargs = JSON.parseObject(data).getJSONArray("params")
        varargs.size() == 5
        varargs.get(0) == 'one'
        varargs.get(1) == 't/wo'
        varargs.get(2) == 't/hree'
        varargs.get(3) == '/four'
        varargs.get(4) == 'five/'
    }

    def "Invoke /test/json-params-varargs/1%20/%202/one/t%20wo/t%20hree/%20four/five%20 testing varargs decoding space"() {
        given:
        def uri = "/test/json-params-varargs/1%20/%202/one/t%20wo/t%20hree/%20four/five%20"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("param1") == '1 '
        and:
        JSON.parseObject(data).get("param2") == ' 2'
        and:
        def varargs = JSON.parseObject(data).getJSONArray("params")
        varargs.size() == 5
        varargs.get(0) == 'one'
        varargs.get(1) == 't wo'
        varargs.get(2) == 't hree'
        varargs.get(3) == ' four'
        varargs.get(4) == 'five '
    }

    def "Invoke /test/json-param testing param with only delimiter"() {
        given:
        def uri = "/test/json-param/%2F%2F%2F"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == '///'
    }

    def "Invoke /test/json-param testing param with only space"() {
        given:
        def uri = "/test/json-param/%20%20%20"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        JSON.parseObject(data).get("test") == '   '
    }
}
