/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.web.http

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Wait
import sirius.kernel.commons.Watch
import sirius.web.dispatch.TestDispatcher
import java.net.HttpURLConnection
import java.net.URI
import java.time.Duration
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Covers the upstream half of the tunnel back-pressure bridge.
 *
 * [TunnelHandler] pauses the upstream connection with `setAutoRead(false)` whenever the client
 * cannot keep up and re-enables it when the bridge is removed. Both of those happen as tasks on
 * *different* event loops, while the upstream connection itself belongs to AsyncHttpClient's
 * connection pool and outlives the request that paused it. `mirrorWritabilityToUpstream` only
 * checks `upstream.isOpen()` - never whether the channel still belongs to the request - and an
 * idle pooled connection is open.
 *
 * A connection left paused is unrecoverable for the request that picks it up next:
 * `installBackpressureBridge` (which re-applies the current writability) is only reached from
 * `onHeadersReceived`, and no headers can be received while reading is disabled. The request
 * therefore hangs until AsyncHttpClient's `readTimeout` closes the connection - one minute in
 * production, which is what the 14s-median / 59.7s-max "Long running tunneling" warnings on
 * `oxomi.cos.scireum.com` look like.
 *
 * The existing tests in `WebServerTest` cover the *client* side of the bridge (full delivery to a
 * slow consumer, and no handler leak across keep-alive). Neither observes the upstream channel
 * after the request ends, which is the seam these tests close.
 */
@ExtendWith(SiriusExtension::class)
class TunnelUpstreamPoolTest {

    @AfterEach
    fun restoreTunnelClient() {
        TunnelPoolProbe.restore()
    }

    /**
     * Reads a response in small blocks with a pause between them, so the server side runs out of
     * writable buffer, and the bridge has to pause the upstream repeatedly.
     */
    private fun drainSlowly(uri: String, blockSize: Int = 8 * 1024, pauseMillis: Int = 1): Int {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        connection.connect()

        var totalBytes = 0
        val block = ByteArray(blockSize)
        var read: Int
        try {
            connection.inputStream.use { input ->
                while (input.read(block).also { read = it } > 0) {
                    totalBytes += read
                    Wait.millis(pauseMillis)
                }
            }
        } finally {
            connection.disconnect()
        }

        return totalBytes
    }

    /**
     * Asserts the invariant directly: a pooled upstream connection must always be readable.
     *
     * This is the assertion a fix has to satisfy, and it does not depend on any timing threshold.
     */
    @Test
    fun `Pooled upstream connections are never left with reading disabled`() {
        TunnelPoolProbe.install(1, Duration.ofSeconds(5))

        repeat(ROUNDS) {
            // The contention-controlled endpoint exercises the bridge across many pause/resume
            // cycles, the burst endpoint completes its upstream response (and pools the
            // connection) while this client is still draining.
            assertEquals(TestDispatcher.STREAMING_PAYLOAD_TOTAL_BYTES, drainSlowly(STREAMING_TUNNEL))
            assertEquals(TestDispatcher.BURST_PAYLOAD_TOTAL_BYTES, drainSlowly(BURST_TUNNEL, 4 * 1024, 4))
            WebServerTest.callAndRead(SMALL_TUNNEL)
        }

        assertTrue(
            TunnelPoolProbe.reusedConnections() > 0,
            "The test never re-used a pooled connection, so it cannot have exercised the bug."
        )
        assertTrue(
            TunnelPoolProbe.stuckWhileIdle().isEmpty(),
            "Pooled upstream connections were still unable to read 2s after being pooled, so they"
                    + " are unusable for any future request:\n"
                    + TunnelPoolProbe.stuckWhileIdle().joinToString("\n")
                    + "\n(transient observations: ${TunnelPoolProbe.pausedWhileIdle().size})"
        )
        assertTrue(
            TunnelPoolProbe.pausedOnPoll().isEmpty(),
            "Paused upstream connections were handed out to new requests, which can only end in a"
                    + " read timeout:\n" + TunnelPoolProbe.pausedOnPoll().joinToString("\n")
        )
        assertTrue(
            TunnelPoolProbe.pausedOnOffer().isEmpty(),
            "Upstream connections were returned to the pool while still paused:\n"
                    + TunnelPoolProbe.pausedOnOffer().joinToString("\n")
        )
    }

    /**
     * Asserts the consequence, with the leak injected rather than raced for.
     *
     * The first request leaves a pooled connection behind, which the probe then puts into exactly
     * the state the leak produces. With the pool pinned to a single connection, the second request
     * has to re-use it. Today nothing repairs the flag, so no response can arrive until
     * AsyncHttpClient's read timeout closes the connection - the production timeout is 60s, which
     * is what the 14s-median / 59.7s-max `Long running tunneling` warnings look like.
     *
     * Enabling reading in `onConnectionPooled` fixes this and makes the test pass.
     */
    @Test
    fun `A pooled connection with reading disabled is repaired before it is reused`() {
        TunnelPoolProbe.install(1, READ_TIMEOUT)

        // Establishes and pools a connection, then leaves it behind in the broken state.
        WebServerTest.callAndRead(SMALL_TUNNEL)
        TunnelPoolProbe.poisonPooledConnections(true)
        WebServerTest.callAndRead(SMALL_TUNNEL)
        Wait.millis(POISON_SETTLE_MILLIS)

        val reusedBefore = TunnelPoolProbe.reusedConnections()
        val watch = Watch.start()
        // A stalled tunnel does not just take long, it ultimately fails once the read timeout
        // closes the connection - so the failure has to be caught to be reported properly.
        var tunnelFailure: Exception? = null
        try {
            WebServerTest.callAndRead(SMALL_TUNNEL)
        } catch (exception: Exception) {
            tunnelFailure = exception
        }
        val elapsed = watch.elapsedMillis()

        assertTrue(
            TunnelPoolProbe.reusedConnections() > reusedBefore,
            "Precondition not reached: the timed request opened a fresh connection instead of"
                    + " re-using the pooled one, so nothing was tested."
        )
        assertTrue(
            TunnelPoolProbe.pausedOnPoll().isNotEmpty(),
            "Precondition not reached: the pooled connection was readable when it was handed out,"
                    + " so the injected pause never took effect."
        )
        assertTrue(
            tunnelFailure == null && elapsed <= STALL_THRESHOLD_MILLIS,
            "A tunnel which re-used a pooled connection with reading disabled took ${elapsed}ms"
                    + " (threshold ${STALL_THRESHOLD_MILLIS}ms, read timeout"
                    + " ${READ_TIMEOUT.toMillis()}ms)"
                    + (tunnelFailure?.let { " and then failed with ${it.javaClass.simpleName}: ${it.message}" } ?: "")
                    + ". Nothing re-enabled reading on that connection, so no response could"
                    + " arrive until the timeout closed it."
        )
    }

    // Note: a test that reproduces the *whole* chain end to end - a real back-pressure pause
    // leaking onto a pooled connection AND a second request then stalling on it - cannot be built
    // in-process. Keeping a consumer unwritable needs a payload of several MiB, but at that size
    // the bridge pauses the upstream before its response completes, so the connection is never
    // pooled; a payload small enough to complete in one burst vanishes into the loopback socket
    // buffers, which makes the consumer writable again and undoes the pause. The two tests above
    // therefore split the chain: the first shows the state occurs, the second shows it is fatal.

    companion object {
        private const val STREAMING_TUNNEL = "/tunnel/streaming-payload"
        private const val BURST_TUNNEL = "/tunnel/burst-payload"
        private const val SMALL_TUNNEL = "/tunnel/test"

        /**
         * The leak needs a pause to be queued while the connection is being pooled, so a single
         * round can miss it. A handful of rounds keep the test quick while making a hit likely.
         */
        private const val ROUNDS = 5

        private val READ_TIMEOUT: Duration = Duration.ofSeconds(5)

        /**
         * Well above a healthy loopback tunnel (single-digit milliseconds) and well below the read
         * timeout, so the assertion separates "slow machine" from "not reading at all".
         */
        private const val STALL_THRESHOLD_MILLIS = 2_000L

        /** Must outlast the probe's injection delay, so the pause is in place before we measure. */
        private const val POISON_SETTLE_MILLIS = 800

    }
}
