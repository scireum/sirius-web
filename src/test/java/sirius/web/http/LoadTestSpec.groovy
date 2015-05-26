/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.google.common.io.ByteStreams
import sirius.kernel.BaseSpecification
import sirius.kernel.commons.Watch
import sirius.kernel.health.Average

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/**
 * Simulates a bunch of "real" (outside) requests through netty and sirius.
 * <p>
 * This ensures a basic performance profile and also makes sure no trivial race conditions or memory leaks
 * are added.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2015/05
 */
class LoadTestSpec extends BaseSpecification {

    def "Invoke /system/ok 20000x"() {
        when:
        for (int i = 0; i < 10; i++) {
            call("/system/ok", 1000, 16);

        }
        Watch w = Watch.start();
        Average avg = new Average();
        for (int i = 0; i < 20; i++) {
            w.reset();
            call("/system/ok", 1000, 16);
            avg.addValue(w.elapsed(TimeUnit.MILLISECONDS, true));

        }
        WebServer.LOG.INFO("Executing %s resulted in %1.2f RPS", "/system/ok", 1000 / (avg.getAvg() / 1000d));
        then:
        avg.getAvg() < 1000
    }

    def "Invoke /system/info 20000x"() {
        when:
        for (int i = 0; i < 10; i++) {
            call("/system/info", 1000, 32);
        }
        Watch w = Watch.start();
        Average avg = new Average();
        for (int i = 0; i < 10; i++) {
            w.reset();
            call("/system/info", 1000, 32);
            avg.addValue(w.elapsed(TimeUnit.MILLISECONDS, true));

        }
        WebServer.LOG.INFO("Executing %s resulted in %1.2f RPS", "/system/info", 1000 / (avg.getAvg() / 1000d));
        then:
        avg.getAvg() < 1000
    }

    def call(String uri, int count, int parallelism) {
        Watch w = Watch.start();
        ExecutorService exec = Executors.newFixedThreadPool(parallelism);
        for (int i = 0; i < count; i++) {
            exec.execute({
                URLConnection c = new URL("http://localhost:9999" + uri).openConnection();
                c.connect();
                ByteStreams.toByteArray(c.getInputStream());
            });
        }
        exec.shutdown();
        exec.awaitTermination(1, TimeUnit.MINUTES);
        return w.elapsedMillis();
    }

}
