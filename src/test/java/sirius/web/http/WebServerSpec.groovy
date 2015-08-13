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
import sirius.kernel.commons.Strings
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
class WebServerSpec extends BaseSpecification {

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
        ExecutorService exec = Executors.newFixedThreadPool(parallelism);
        Watch w = Watch.start();
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

    def callAndRead(String uri, Map outHeaders, Map expectedHeaders) {
        URLConnection c = new URL("http://localhost:9999" + uri).openConnection();
        outHeaders.each { k, v -> c.addRequestProperty(k, v); }
        c.connect();
        def result = new String(ByteStreams.toByteArray(c.getInputStream()), Charsets.UTF_8);
        expectedHeaders.each { k, v ->
            if (!Strings.areEqual(c.getHeaderField(k), v)) {
                throw new IllegalStateException("Header: " + k + " was " + c.getHeaderField(k) + " instead of " + v);
            }
        }

        return result;
    }

    def "Invoke /assets/test.css to test"() {
        given:
        def uri = "/assets/test.css";
        def headers = ['accept-encoding': 'gzip'];
        // File is too small to be compressed!
        def expectedHeaders = ['content-encoding': null]
        when:
        def data = callAndRead(uri, headers, expectedHeaders);
        then:
        "body { background-color: #000000; }" == data
    }

    def "Invoke /assets/test_large.css"() {
        given:
        def uri = "/assets/test_large.css";
        def expectedHeaders = ['content-encoding': null]
        when:
        def data = callAndRead(uri, null, expectedHeaders);
        then:
        60314 == data.length()
    }

    def "Invoke /assets/test_large.css with GZIP"() {
        given:
        def uri = "/assets/test_large.css";
        def headers = ['accept-encoding': 'gzip'];
        def expectedHeaders = ['content-encoding': 'gzip']
        when:
        def data = callAndRead(uri, headers, expectedHeaders);
        then:
        // URLConnection does not understand GZIP and therefore does not unzip... :-(
        1298 == data.length()
    }

    /**
     * Call a small service which result fits into a single response chunk...
     */
    def "Invoke /service/json/test"() {
        given:
        def uri = "/service/json/test";
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders);
        then:
        '{"test":true}' == data
    }

    /**
     * Call a large service to test buffer-based output streams
     */
    def "Invoke /service/json/test_large"() {
        given:
        def uri = "/service/json/test_large";
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders);
        then:
        // Size should be contents of large test file plus json overhead and escaping....
        60543 == data.length()
    }

    /**
     * Call a controller which tunnels a small file
     */
    def "Invoke /tunnel/test"() {
        given:
        def uri = "/tunnel/test";
        def expectedHeaders = ['content-type': 'text/test']
        when:
        def data = callAndRead(uri, null, expectedHeaders);
        then:
        '{"test":true}' == data
    }

    /**
     * Call a controller which tunnels a large file
     */
    def "Invoke /tunnel/test_large"() {
        given:
        def uri = "/tunnel/test_large";
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders);
        then:
        // Size should be contents of large test file plus json overhead and escaping....
        60543 == data.length()
    }

    /**
     * Call a controller which uses predispatching
     */
    def "Invoke /test/presidpatch with POST"() {
        given:
        def HttpURLConnection u = new URL("http://localhost:9999/test/predispatch").openConnection();
        and:
        def testByteArray = "Hello Service".getBytes()
        when:
        u.setRequestMethod("POST");
        u.setDoInput(true);
        u.setDoOutput(true);
        def out = u.getOutputStream();
        for(int i = 0; i < 1024; i++) {
            out.write(testByteArray);
        }
        out.close();
        def result = new String(ByteStreams.toByteArray(u.getInputStream()), Charsets.UTF_8);
        then:
        String.valueOf(testByteArray.length * 1024) == result;
    }

    /**
     * Ensure that predispatching does not trigger on GET requests
     */
    def "Invoke /test/presidpatch with GET"() {
        given:
        def HttpURLConnection u = new URL("http://localhost:9999/test/predispatch").openConnection();
        when:
        u.setRequestMethod("GET");
        then:
        u.getResponseCode() == 404;
    }

}
