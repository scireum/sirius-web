/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.google.common.base.Charsets
import com.google.common.io.ByteStreams
import sirius.kernel.BaseSpecification
import sirius.kernel.commons.ValueHolder
import sirius.kernel.commons.Watch

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/**
 * Created by aha on 26.05.15.
 */
class LoadTestSpec extends BaseSpecification {

    def "Invoke /system/ok 10000x"() {
        when:
        call("/system/ok", 10000, 16);
        Watch w = Watch.start();
        def result = call("/system/ok", 10000, 16);
        then:
        "OK" == result
        w.elapsedMillis() < 10000
    }

    def "Invoke /system/info 10000x"() {
        when:
        call("/system/info", 10000, 32);
        Watch w = Watch.start();
        call("/system/info", 10000, 32);
        then:
        w.elapsedMillis() < 10000
    }

    def call(String uri, int count, int parallelism) {
        Watch w = Watch.start();
        ValueHolder<String> vh = ValueHolder.of(null);
        ExecutorService exec = Executors.newFixedThreadPool(parallelism);
        for (int i = 0; i < 10000; i++) {
            exec.execute({
                URLConnection c = new URL("http://localhost:9999" + uri).openConnection();
                c.connect();
                vh.set(new String(ByteStreams.toByteArray(c.getInputStream()), Charsets.UTF_8));
            });
        }
        exec.shutdown();
        exec.awaitTermination(1, TimeUnit.MINUTES);
        WebServer.LOG.INFO("Calling %s %d time in %d threads took %s. This equals %1.2f RPS", uri, count, parallelism, w.duration(), (count / (w.elapsed(TimeUnit.MILLISECONDS, false) / 1000d)));
        return vh.get();
    }

}
