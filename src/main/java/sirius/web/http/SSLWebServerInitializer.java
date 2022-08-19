/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.socket.SocketChannel;
import io.netty.handler.ssl.SslHandler;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.health.Log;

import javax.net.ssl.ExtendedSSLSession;
import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SNIHostName;
import javax.net.ssl.SNIServerName;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.StandardConstants;
import javax.net.ssl.X509ExtendedKeyManager;
import java.io.InputStream;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.KeyStore;
import java.security.Principal;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.List;

/**
 * Creates a new pipeline for processing incoming requests of the HTTPS web server.
 */
class SSLWebServerInitializer extends WebServerInitializer {

    private static final String[] EMPTY_STRING_ARRAY = new String[0];
    private final SSLContext context;

    @ConfigValue("http.ssl.alias")
    private static String defaultAlias;

    @ConfigValue("http.ssl.keystore")
    private static String keystore;

    @ConfigValue("http.ssl.password")
    private static String password;

    @ConfigValue("http.ssl.ephemeralDHKeySize")
    private static int ephemeralDHKeySize;

    @ConfigValue("http.ssl.protocols")
    private static List<String> protocols;

    @ConfigValue("http.ssl.ciphers")
    private static List<String> ciphers;

    public static final Log LOG = Log.get("ssl");

    /**
     * Adds support for SNI for the SSL engine.
     */
    static class SniKeyManager extends X509ExtendedKeyManager {
        private final X509ExtendedKeyManager keyManager;

        SniKeyManager(X509ExtendedKeyManager keyManager) {
            this.keyManager = keyManager;
        }

        @Override
        public String[] getClientAliases(String keyType, Principal[] issuers) {
            throw new UnsupportedOperationException();
        }

        @Override
        public String chooseClientAlias(String[] keyType, Principal[] issuers, Socket socket) {
            throw new UnsupportedOperationException();
        }

        @Override
        public String chooseEngineClientAlias(String[] keyType, Principal[] issuers, SSLEngine engine) {
            throw new UnsupportedOperationException();
        }

        @Override
        public String[] getServerAliases(String keyType, Principal[] issuers) {
            String[] serverAliases = keyManager.getServerAliases(keyType, issuers);
            if (LOG.isFINE()) {
                if (serverAliases == null) {
                    LOG.FINE("Server aliases are null...");
                } else {
                    LOG.FINE("Server aliases are: %s", Strings.join(", ", serverAliases));
                }
            }
            return serverAliases;
        }

        @Override
        public String chooseServerAlias(String keyType, Principal[] issuers, Socket socket) {
            throw new UnsupportedOperationException();
        }

        @Override
        public String chooseEngineServerAlias(String keyType, Principal[] issuers, SSLEngine engine) {
            ExtendedSSLSession session = (ExtendedSSLSession) engine.getHandshakeSession();
            // Pick first SNIHostName in the list of SNI names.
            String hostname = null;
            for (SNIServerName name : session.getRequestedServerNames()) {
                if (name.getType() == StandardConstants.SNI_HOST_NAME) {
                    hostname = ((SNIHostName) name).getAsciiName();
                    break;
                }
            }

            if (hostname != null && LOG.isFINE()) {
                LOG.FINE(
                        "Choosing %s as SNI hostname, searching for certificate and key (SSL chooseEngineServerAlias)...",
                        hostname);
            }

            // If we got given a hostname over SNI, check if we have a cert and key for that hostname. If so, we use it.
            // Otherwise, we fall back to the default certificate.
            if (hostname != null && (getCertificateChain(hostname) != null && getPrivateKey(hostname) != null)) {
                if (LOG.isFINE()) {
                    LOG.FINE("Using '%s' as hostname (SSL chooseEngineServerAlias)...", hostname);
                }
                return hostname;
            } else {
                if (LOG.isFINE()) {
                    LOG.FINE("Using the default alias '%s' as hostname (SSL chooseEngineServerAlias)...", hostname);
                }
                return defaultAlias;
            }
        }

        @Override
        public X509Certificate[] getCertificateChain(String alias) {
            X509Certificate[] certificateChain = keyManager.getCertificateChain(alias);
            if (certificateChain == null) {
                LOG.FINE("No certificate chain found for: %s", alias);
            } else {
                if (LOG.isFINE()) {
                    LOG.FINE("Certificate chain for %s has %d entries...", alias, certificateChain.length);
                }
            }
            return certificateChain;
        }

        @Override
        public PrivateKey getPrivateKey(String alias) {
            PrivateKey privateKey = keyManager.getPrivateKey(alias);
            if (privateKey == null) {
                LOG.FINE("No private key found for: %s", alias);
            } else {
                if (LOG.isFINE()) {
                    LOG.FINE("Private key for %s is: %s", alias, privateKey);
                }
            }

            return privateKey;
        }
    }

    SSLWebServerInitializer() throws Exception {
        KeyStore store = KeyStore.getInstance("JKS");
        try (InputStream is = Files.newInputStream(Paths.get(keystore))) {
            store.load(is, password.toCharArray());
        }
        KeyManagerFactory factory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        factory.init(store, password.toCharArray());

        X509ExtendedKeyManager x509KeyManager = null;
        for (KeyManager keyManager : factory.getKeyManagers()) {
            if (keyManager instanceof X509ExtendedKeyManager x509ExtendedKeyManager) {
                x509KeyManager = x509ExtendedKeyManager;
            }
        }
        if (x509KeyManager == null) {
            throw new Exception("KeyManagerFactory did not create an X509ExtendedKeyManager");
        }
        System.setProperty("jdk.tls.ephemeralDHKeySize", String.valueOf(ephemeralDHKeySize));
        SniKeyManager sniKeyManager = new SniKeyManager(x509KeyManager);
        context = SSLContext.getInstance("TLS");
        context.init(new KeyManager[]{sniKeyManager}, null, null);
    }

    @Override
    protected boolean isSSL() {
        return true;
    }

    @Override
    public void initChannel(SocketChannel ch) throws Exception {
        SSLEngine engine = context.createSSLEngine();
        engine.setUseClientMode(false);
        if (!ciphers.isEmpty()) {
            engine.setEnabledCipherSuites(ciphers.toArray(EMPTY_STRING_ARRAY));
        }
        if (!protocols.isEmpty()) {
            engine.setEnabledProtocols(protocols.toArray(EMPTY_STRING_ARRAY));
        }
        ch.pipeline().addFirst(new SslHandler(engine));
        super.initChannel(ch);
    }
}
