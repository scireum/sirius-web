/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Register;
import sirius.web.http.ChunkedOutputStream;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;

import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

@Register
public class TestDispatcher implements WebDispatcher {

    @Override
    public int getPriority() {
        return 100;
    }

    /**
     * Number of 18-byte chunks emitted by {@link #STREAMING_PAYLOAD_PATH}. Yields ~20 MiB per
     * response, well above Netty's default high watermark (64 KiB) so that any test which slows
     * down the consumer forces the tunnel's back-pressure bridge through multiple writability
     * transitions.
     */
    public static final int STREAMING_PAYLOAD_CHUNKS = 1_200_000;
    public static final int STREAMING_PAYLOAD_TOTAL_BYTES = STREAMING_PAYLOAD_CHUNKS * 18;
    public static final String STREAMING_PAYLOAD_PATH = "/test/streaming-payload";

    /**
     * A payload that exceeds Netty's 64 KiB high watermark, so the tunnel's back-pressure bridge
     * has to engage, yet is small enough for the upstream to hand over in a single burst and thus
     * finish (and pool its connection) while the consuming client is still draining.
     */
    public static final int BURST_PAYLOAD_CHUNKS = 32_768;
    public static final int BURST_PAYLOAD_TOTAL_BYTES = BURST_PAYLOAD_CHUNKS * 18;
    public static final String BURST_PAYLOAD_PATH = "/test/burst-payload";

    @Override
    public DispatchDecision dispatch(WebContext ctx) throws Exception {
        if ("/large-blocking-calls".equalsIgnoreCase(ctx.getRequestedURI())) {
            // See WebServerSpec->"Invoke /large-blocking-calls with GET" to the appropriate test and explanation...
            OutputStream out = ctx.respondWith().outputStream(HttpResponseStatus.OK, "text/plain");
            for (int i = 0; i < 10000000; i++) {
                out.write("THISISLARGECONTENT".getBytes(StandardCharsets.UTF_8));
            }
            out.close();
            return DispatchDecision.DONE;
        }
        if (BURST_PAYLOAD_PATH.equalsIgnoreCase(ctx.getRequestedURI())) {
            // The deliberate opposite of STREAMING_PAYLOAD_PATH: no contention control, so the
            // whole payload is dumped into the outbound buffer at once and the tunnel's upstream
            // response completes (and its connection is returned to the pool) while the consuming
            // client is still draining. That is the window in which a back-pressure pause can
            // land on an already pooled connection.
            OutputStream burst = ctx.respondWith().outputStream(HttpResponseStatus.OK, "text/plain");
            byte[] burstChunk = "THISISLARGECONTENT".getBytes(StandardCharsets.UTF_8);
            for (int i = 0; i < BURST_PAYLOAD_CHUNKS; i++) {
                burst.write(burstChunk);
            }
            burst.close();
            return DispatchDecision.DONE;
        }
        if (STREAMING_PAYLOAD_PATH.equalsIgnoreCase(ctx.getRequestedURI())) {
            // Contention control makes the dispatcher block until each chunk has been written to
            // the socket, which emulates what a real remote backend would see under TCP flow
            // control. Without it, the dispatcher would dump the entire payload into its own Netty
            // outbound buffer on the same JVM, masking any back-pressure applied on the tunnel
            // side.
            ChunkedOutputStream out = ctx.respondWith()
                                         .outputStream(HttpResponseStatus.OK, "text/plain")
                                         .enableContentionControl();
            byte[] chunk = "THISISLARGECONTENT".getBytes(StandardCharsets.UTF_8);
            for (int i = 0; i < STREAMING_PAYLOAD_CHUNKS; i++) {
                out.write(chunk);
            }
            out.close();
            return DispatchDecision.DONE;
        }
        if ("/redispatch".equalsIgnoreCase(ctx.getRequestedURI())) {
            // See WebServerSpec->"Redispatching works" to the appropriate test and explanation...
            ctx.withCustomPath("/api/test");
            return DispatchDecision.RESTART;
        } else {
            return DispatchDecision.CONTINUE;
        }
    }
}
