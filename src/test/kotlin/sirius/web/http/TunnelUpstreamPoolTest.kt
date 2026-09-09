/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.web.http

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Wait
import sirius.kernel.commons.Watch
import java.time.Duration
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

    companion object {
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
