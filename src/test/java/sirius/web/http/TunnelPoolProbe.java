/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.Channel;
import io.netty.util.HashedWheelTimer;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.AsyncHttpClientConfig;
import org.asynchttpclient.DefaultAsyncHttpClientConfig;
import org.asynchttpclient.Dsl;
import org.asynchttpclient.channel.ChannelPool;
import org.asynchttpclient.netty.channel.DefaultChannelPool;
import sirius.kernel.commons.Explain;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;

/**
 * Swaps the static HTTP client used by {@link Response#tunnel(String)} for one whose connection
 * pool can be observed by tests.
 * <p>
 * {@link TunnelHandler} applies back-pressure by toggling {@code autoRead} on the <b>upstream</b>
 * channel, which is a connection owned and re-used by AsyncHttpClient's pool. Nothing in the
 * existing test suite looks at the state of that channel once the request which paused it has
 * finished, so this probe records every channel that enters or leaves the pool while reading is
 * disabled. A channel handed out in that state can never complete its next response: no bytes are
 * read, so {@code onHeadersReceived} - and with it the code that would re-enable reading - never
 * runs.
 * <p>
 * The pool is pinned to a single connection per host by default so that connection re-use is
 * deterministic rather than incidental.
 */
public final class TunnelPoolProbe {

    private static AsyncHttpClient previousClient;
    private static AsyncHttpClient probeClient;
    private static HashedWheelTimer timer;
    private static RecordingChannelPool pool;

    private TunnelPoolProbe() {
    }

    /**
     * Installs the probe.
     *
     * @param maxConnectionsPerHost the size of the connection pool. Use <tt>1</tt> to force every
     *                              request onto the same upstream connection.
     * @param readTimeout           how long AsyncHttpClient waits for data before it aborts. The
     *                              production default is one minute, which would make a stalled
     *                              test hang for that long, so tests pass something short.
     */
    public static void install(int maxConnectionsPerHost, Duration readTimeout) {
        previousClient = Response.asyncClient;

        timer = new HashedWheelTimer();
        DefaultAsyncHttpClientConfig.Builder builder = Dsl.config()
                                                          .setCookieStore(null)
                                                          // mirrors Response.getAsyncClient()
                                                          .setRequestTimeout(Duration.ofSeconds(-1))
                                                          .setReadTimeout(readTimeout)
                                                          .setKeepAlive(true)
                                                          .setMaxConnectionsPerHost(maxConnectionsPerHost);

        AsyncHttpClientConfig poolConfig = builder.build();
        pool = new RecordingChannelPool(new DefaultChannelPool(poolConfig, timer));

        probeClient = Dsl.asyncHttpClient(builder.setChannelPool(pool));
        Response.asyncClient = probeClient;
    }

    /**
     * Restores the original client. Must be called from a teardown block, as the field is static
     * and would otherwise leak into unrelated tests.
     */
    public static void restore() {
        Response.asyncClient = previousClient;
        previousClient = null;

        if (probeClient != null) {
            try {
                probeClient.close();
            } catch (Exception _) {
                // Nothing we can do about a client which refuses to close, and failing here would
                // mask the actual assertion of the test...
            }
            probeClient = null;
        }
        if (timer != null) {
            timer.stop();
            timer = null;
        }
        pool = null;
    }

    /**
     * Lists the upstream connections which were <b>returned to</b> the pool while reading was
     * disabled.
     *
     * @return a description per occurrence, empty if there was none
     */
    public static List<String> pausedOnOffer() {
        return pool == null ? Collections.emptyList() : pool.snapshot(pool.pausedOnOffer);
    }

    /**
     * Lists the upstream connections which were <b>handed out of</b> the pool while reading was
     * disabled. Every entry here is a request that cannot make progress.
     *
     * @return a description per occurrence, empty if there was none
     */
    public static List<String> pausedOnPoll() {
        return pool == null ? Collections.emptyList() : pool.snapshot(pool.pausedOnPoll);
    }

    /**
     * Simulates the leak deterministically by disabling reading on every connection as it is
     * returned to the pool.
     * <p>
     * {@code Pooled upstream connections are never left with reading disabled} shows that this
     * state does occur on its own, but only for a few hundred milliseconds and only when the
     * timing works out. Injecting it directly makes the <i>consequence</i> testable without
     * depending on that race: a connection in this state cannot deliver a response, because
     * nothing in AsyncHttpClient re-enables the flag and {@code installBackpressureBridge} is only
     * reached once headers have arrived.
     *
     * @param poison whether to disable reading on pooled connections
     */
    public static void poisonPooledConnections(boolean poison) {
        if (pool != null) {
            pool.poison = poison;
        }
    }

    /**
     * Lists the upstream connections which were observed <b>sitting idle in the pool</b> with
     * reading disabled. This is the defect state itself: the connection belongs to nobody, yet it
     * will not read, so whichever request picks it up next cannot make progress.
     *
     * @return a description per occurrence, empty if there was none
     */
    public static List<String> pausedWhileIdle() {
        return pool == null ? Collections.emptyList() : pool.snapshot(pool.pausedWhileIdle);
    }

    /**
     * Lists pooled connections still unable to read {@value #STUCK_THRESHOLD_MILLIS}ms after being
     * pooled. A brief pause is benign - the owning request may simply not have released the
     * connection yet - but one that persists is a connection nobody can use again.
     *
     * @return a description per occurrence, empty if there was none
     */
    public static List<String> stuckWhileIdle() {
        if (pool == null) {
            return Collections.emptyList();
        }
        return pool.snapshot(pool.pausedWhileIdle)
                   .stream()
                   .filter(entry -> entry.contains("after " + STUCK_THRESHOLD_MILLIS + "ms"))
                   .toList();
    }

    /**
     * @return the number of times a pooled connection was re-used, so a test can assert that it
     * actually exercised connection re-use instead of silently opening fresh connections
     */
    public static int reusedConnections() {
        return pool == null ? 0 : pool.reused.size();
    }

    /**
     * When to re-inspect a pooled connection. Spread out so a late pause is caught regardless of
     * how long the event loop takes to get to it.
     */
    private static final int[] IDLE_PROBE_DELAYS_MILLIS = {20, 100, 400, 2_000};

    /**
     * How long after pooling the injected pause is applied - long enough to be past the restore
     * performed when the previous request's bridge is removed.
     */
    private static final int POISON_DELAY_MILLIS = 400;

    /**
     * The probe delay beyond which a pause is considered stuck rather than transient.
     */
    private static final int STUCK_THRESHOLD_MILLIS = 2_000;

    private static final class RecordingChannelPool implements ChannelPool {

        private final ChannelPool delegate;
        private final List<String> pausedOnOffer = Collections.synchronizedList(new ArrayList<>());
        private final List<String> pausedOnPoll = Collections.synchronizedList(new ArrayList<>());
        private final List<String> pausedWhileIdle = Collections.synchronizedList(new ArrayList<>());
        private final List<String> reused = Collections.synchronizedList(new ArrayList<>());
        /**
         * Channels currently sitting in the pool. An idle check must only report a channel that is
         * still pooled - once it has been handed to a new request, that request may pause it
         * perfectly legitimately, and reporting it then would be a false positive.
         */
        private final Set<Channel> pooled = ConcurrentHashMap.newKeySet();
        private volatile boolean poison;

        private RecordingChannelPool(ChannelPool delegate) {
            this.delegate = delegate;
        }

        private List<String> snapshot(List<String> source) {
            synchronized (source) {
                return new ArrayList<>(source);
            }
        }

        @Override
        @SuppressWarnings("resource")
        @Explain("The eventLoop gets managed by Netty.")
        public boolean offer(Channel channel, Object partitionKey) {
            if (!channel.config().isAutoRead()) {
                pausedOnOffer.add(describe("returned to pool", channel, partitionKey));
            }
            boolean accepted = delegate.offer(channel, partitionKey);
            if (!accepted) {
                return false;
            }

            pooled.add(channel);
            if (poison) {
                // Deliberately delayed: the finishing request's bridge removal restores autoRead
                // *after* the connection was pooled, so injecting immediately would simply be
                // overwritten by it. This lands once the connection is genuinely idle.
                channel.eventLoop()
                       .schedule(() -> channel.config().setAutoRead(false), POISON_DELAY_MILLIS, TimeUnit.MILLISECONDS);
            }
            // A pause queued by the finishing request can only land on the upstream event loop
            // after it was pooled, so sampling once at offer time is not enough. These checks
            // run on the channel's own event loop, i.e. after any such pending task.
            for (int delay : IDLE_PROBE_DELAYS_MILLIS) {
                watchIdleChannel(channel, partitionKey, delay);
            }
            return true;
        }

        @SuppressWarnings("resource")
        @Explain("The eventLoop gets managed by Netty.")
        private void watchIdleChannel(Channel channel, Object partitionKey, int delayMillis) {
            channel.eventLoop().schedule(() -> {
                if (channel.isOpen() && pooled.contains(channel) && !channel.config().isAutoRead()) {
                    pausedWhileIdle.add(describe("idle in pool after " + delayMillis + "ms", channel, partitionKey));
                }
            }, delayMillis, TimeUnit.MILLISECONDS);
        }

        @Override
        public Channel poll(Object partitionKey) {
            Channel channel = delegate.poll(partitionKey);
            if (channel != null) {
                pooled.remove(channel);
                reused.add(channel.id().asShortText());
                if (!channel.config().isAutoRead()) {
                    pausedOnPoll.add(describe("handed out of pool", channel, partitionKey));
                }
            }
            return channel;
        }

        private String describe(String what, Channel channel, Object partitionKey) {
            return what + " with autoRead=false: channel=" + channel.id().asShortText() + ", partition=" + partitionKey;
        }

        @Override
        public boolean removeAll(Channel channel) {
            pooled.remove(channel);
            return delegate.removeAll(channel);
        }

        @Override
        public boolean isOpen() {
            return delegate.isOpen();
        }

        @Override
        public void destroy() {
            delegate.destroy();
        }

        @Override
        public void flushPartitions(Predicate<Object> predicate) {
            delegate.flushPartitions(predicate);
        }

        @Override
        public java.util.Map<String, Long> getIdleChannelCountPerHost() {
            return delegate.getIdleChannelCountPerHost();
        }
    }
}
