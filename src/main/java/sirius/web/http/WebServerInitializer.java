/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;
import io.netty.handler.stream.ChunkedWriteHandler;
import io.netty.handler.timeout.IdleStateHandler;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.di.PartCollection;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Parts;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Creates a new pipeline for processing incoming requests of the HTTP web server.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
class WebServerInitializer extends ChannelInitializer<SocketChannel> {


    @Parts(WebDispatcher.class)
    private static PartCollection<WebDispatcher> dispatchers;

    @ConfigValue("http.idleTimeout")
    private Duration idleTimeout;

    private static List<WebDispatcher> sortedDispatchers;

    protected WebServerInitializer() {
    }

    @Override
    public void initChannel(SocketChannel ch) throws Exception {
        ChannelPipeline pipeline = ch.pipeline();

        pipeline.addFirst("lowlevel", LowLevelHandler.INSTANCE);
        pipeline.addLast("decoder", new HttpRequestDecoder());
        pipeline.addLast("encoder", new HttpResponseEncoder());
        if (idleTimeout != null && idleTimeout.get(ChronoUnit.SECONDS) > 0) {
            pipeline.addLast("idler",
                             new IdleStateHandler(0, 0, idleTimeout.get(ChronoUnit.SECONDS), TimeUnit.SECONDS));
        }
        pipeline.addLast("compressor", new SmartHttpContentCompressor());
        pipeline.addLast("chunkedWriter", new ChunkedWriteHandler());
        if (WebServerHandler.sortedDispatchers == null) {
            WebServerHandler.sortedDispatchers = getSortedDispatchers();
        }
        pipeline.addLast("handler", new WebServerHandler());
    }

    /*
     * Sorts all available dispatchers by their priority ascending
     */
    protected static List<WebDispatcher> getSortedDispatchers() {
        if (sortedDispatchers == null) {
            PriorityCollector<WebDispatcher> collector = PriorityCollector.create();
            for (WebDispatcher wd : dispatchers.getParts()) {
                collector.add(wd.getPriority(), wd);
            }
            sortedDispatchers = collector.getData();
        }
        return sortedDispatchers;
    }

}
