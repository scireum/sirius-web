/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import org.bouncycastle.asn1.x500.X500Name
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter
import org.bouncycastle.cert.jcajce.JcaX509v3CertificateBuilder
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder
import org.junit.jupiter.api.Test
import java.math.BigInteger
import java.nio.ByteBuffer
import java.security.KeyPairGenerator
import java.security.KeyStore
import java.time.Duration
import java.time.Instant
import java.util.Date
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.SSLEngine
import javax.net.ssl.SSLEngineResult
import javax.net.ssl.SSLSession
import javax.net.ssl.TrustManagerFactory
import javax.net.ssl.X509ExtendedKeyManager
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Provides tests for the [SSLWebServerInitializer.SniKeyManager] which selects the server certificate.
 */
class SniKeyManagerTest {

    @Test
    fun `chooses the fallback alias without a handshake session`() {
        val engine = createServerContext().createSSLEngine()

        assertEquals(ALIAS, createSniKeyManager().chooseEngineServerAlias("EC", null, engine))
    }

    @Test
    fun `resumes a session from a session ticket`() {
        val serverContext = createServerContext()
        val clientContext = createClientContext()

        val initialSession = performHandshake(serverContext, clientContext)
        // Without a cached session ID, the server can only resume the session from the ticket the client offers.
        assertFalse(
            serverContext.serverSessionContext.ids.hasMoreElements(),
            "The server cached the session ID, so the session ticket path would not be exercised"
        )
        val resumedSession = performHandshake(serverContext, clientContext)

        assertContentEquals(initialSession.id, resumedSession.id)
    }

    companion object {
        private const val ALIAS = "default"
        private const val MAX_HANDSHAKE_ROUNDS = 100
        private val PASSWORD = "secret".toCharArray()

        /**
         * Pins TLS 1.2, as only its session ticket path calls the key manager without a handshake session.
         *
         * TLS 1.3 swallows the error and silently falls back to a full handshake. Also, its tickets arrive after
         * the handshake and its session IDs don't reveal a resumption, so [performHandshake] can't cover it anyway.
         */
        private val PROTOCOLS = arrayOf("TLSv1.2")

        private val keyStore: KeyStore = createKeyStore()

        private fun createKeyStore(): KeyStore {
            val keyPair = KeyPairGenerator.getInstance("EC").apply { initialize(256) }.generateKeyPair()
            val subject = X500Name("CN=localhost")
            val now = Instant.now()
            val certificate = JcaX509CertificateConverter().getCertificate(
                JcaX509v3CertificateBuilder(
                    subject,
                    BigInteger.ONE,
                    Date.from(now.minus(Duration.ofDays(1))),
                    Date.from(now.plus(Duration.ofDays(1))),
                    subject,
                    keyPair.public
                ).build(JcaContentSignerBuilder("SHA256withECDSA").build(keyPair.private))
            )

            return KeyStore.getInstance("PKCS12").apply {
                load(null, null)
                setKeyEntry(ALIAS, keyPair.private, PASSWORD, arrayOf(certificate))
            }
        }

        private fun createSniKeyManager(): SSLWebServerInitializer.SniKeyManager {
            val factory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm())
            factory.init(keyStore, PASSWORD)
            return SSLWebServerInitializer.SniKeyManager(factory.keyManagers[0] as X509ExtendedKeyManager, ALIAS)
        }

        private fun createServerContext(): SSLContext {
            return SSLContext.getInstance("TLS").apply { init(arrayOf(createSniKeyManager()), null, null) }
        }

        private fun createClientContext(): SSLContext {
            val factory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
            factory.init(keyStore)
            return SSLContext.getInstance("TLS").apply { init(null, factory.trustManagers, null) }
        }

        /**
         * Performs a TLS handshake between two in-memory engines.
         *
         * The client engine is always created for the same peer, so that a second handshake offers the session
         * ticket which the server issued during the first one.
         *
         * @param serverContext the context to create the server engine from
         * @param clientContext the context to create the client engine from, which also caches the session tickets
         * @return the session negotiated by the client
         */
        private fun performHandshake(serverContext: SSLContext, clientContext: SSLContext): SSLSession {
            val server = serverContext.createSSLEngine().apply {
                useClientMode = false
                enabledProtocols = PROTOCOLS
            }
            val client = clientContext.createSSLEngine("localhost", 9443).apply {
                useClientMode = true
                enabledProtocols = PROTOCOLS
            }

            val clientToServer = ByteBuffer.allocate(client.session.packetBufferSize)
            val serverToClient = ByteBuffer.allocate(server.session.packetBufferSize)
            val clientInput = ByteBuffer.allocate(client.session.applicationBufferSize)
            val serverInput = ByteBuffer.allocate(server.session.applicationBufferSize)
            val noOutput = ByteBuffer.allocate(0)

            client.beginHandshake()
            server.beginHandshake()
            var round = 0
            while (round++ < MAX_HANDSHAKE_ROUNDS && !(isDone(client) && isDone(server))) {
                client.wrap(noOutput, clientToServer)
                runDelegatedTasks(client)
                server.wrap(noOutput, serverToClient)
                runDelegatedTasks(server)

                clientToServer.flip()
                server.unwrap(clientToServer, serverInput)
                clientToServer.compact()
                runDelegatedTasks(server)

                serverToClient.flip()
                client.unwrap(serverToClient, clientInput)
                serverToClient.compact()
                runDelegatedTasks(client)
            }

            assertTrue(isDone(client) && isDone(server), "The handshake did not complete")
            return client.session
        }

        private fun isDone(engine: SSLEngine): Boolean {
            return engine.handshakeStatus == SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING
        }

        private fun runDelegatedTasks(engine: SSLEngine) {
            generateSequence { engine.delegatedTask }.forEach { it.run() }
        }
    }
}
