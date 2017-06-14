/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.buffer.ByteBufAllocator;
import io.netty.handler.ssl.SslContext;
import io.netty.handler.ssl.SslContextBuilder;
import io.netty.handler.ssl.SslProvider;
import org.asynchttpclient.AsyncHttpClientConfig;
import org.asynchttpclient.netty.ssl.InsecureTrustManagerFactory;
import org.asynchttpclient.netty.ssl.SslEngineFactoryBase;
import org.asynchttpclient.util.MiscUtils;

import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLException;
import java.util.Arrays;

/**
 * Rewrite of {@link org.asynchttpclient.netty.ssl.DefaultSslEngineFactory} to make it compatible to Netty 4.1.x
 */
public class SiriusSslEngineFactory extends SslEngineFactoryBase {

    private volatile SslContext sslContext;

    private SslContext buildSslContext(AsyncHttpClientConfig config) throws SSLException {
        if (config.getSslContext() != null) {
            return config.getSslContext();
        }

        SslContextBuilder sslContextBuilder = SslContextBuilder.forClient()
                                                               .sslProvider(config.isUseOpenSsl() ?
                                                                            SslProvider.OPENSSL :
                                                                            SslProvider.JDK)
                                                               .sessionCacheSize(config.getSslSessionCacheSize())
                                                               .sessionTimeout(config.getSslSessionTimeout());

        if (MiscUtils.isNonEmpty(config.getEnabledCipherSuites())) {
            sslContextBuilder.ciphers(Arrays.asList(config.getEnabledCipherSuites()));
        }

        if (config.isAcceptAnyCertificate()) {
            sslContextBuilder.trustManager(InsecureTrustManagerFactory.INSTANCE);
        }

        return configureSslContextBuilder(sslContextBuilder).build();
    }

    @Override
    public SSLEngine newSslEngine(AsyncHttpClientConfig config, String peerHost, int peerPort) {
        // FIXME should be using ctx allocator
        SSLEngine sslEngine = sslContext.newEngine(ByteBufAllocator.DEFAULT, peerHost, peerPort);
        configureSslEngine(sslEngine, config);
        return sslEngine;
    }

    @Override
    public void init(AsyncHttpClientConfig config) throws SSLException {
        sslContext = buildSslContext(config);
    }

    /**
     * The last step of configuring the SslContextBuilder used to create an SslContext when no context is provided in
     * the {@link AsyncHttpClientConfig}. This defaults to no-op and
     * is intended to be overridden as needed.
     *
     * @param builder builder with normal configuration applied
     * @return builder to be used to build context (can be the same object as the input)
     */
    protected SslContextBuilder configureSslContextBuilder(SslContextBuilder builder) {
        // default to no op
        return builder;
    }
}
