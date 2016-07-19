/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.crunchlog;

import com.google.common.collect.Queues;
import sirius.kernel.async.BackgroundLoop;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.health.Log;

import javax.annotation.Nonnull;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * The crunchlog is an effective way to logging events and other statistical information as fast as possible.
 * <p>
 * By default logs are kept in a directory on disc. This can be changed by providing a CrunchLogStore.
 */
public class Crunchlog extends BackgroundLoop {

    public static final Log LOG = Log.get("crunchlog");

    private BlockingQueue<Map<String, Object>> backlog = Queues.newLinkedBlockingDeque(1024);
    private RateLimit logLimit = RateLimit.timeInterval(10, TimeUnit.SECONDS);

    public void log(Map<String, Object> event) {
        if (!backlog.offer(event)) {
            if (logLimit.check()) {
                LOG.WARN("Crunchlog is dropping events as way more events are produced as can be written to disc!");
            }
        }
    }

    @Nonnull
    @Override
    public String getName() {
        return null;
    }

    @Override
    protected void doWork() throws Exception {
        synchronized (backlog) {
            while (true) {
                Map<String, Object> event = backlog.take();
                if (event == null) {
                    return;
                }

//                String line = serializeEvent(event);
//                writeEvent(line);
            }
        }
    }
}
