/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.kernel.BaseSpecification
import sirius.kernel.async.Future
import sirius.kernel.async.Tasks
import sirius.kernel.di.std.Part

import java.time.Duration
import java.time.temporal.TemporalUnit
import java.util.concurrent.TimeUnit

class ScopeInfoSpec extends BaseSpecification {

    @Part
    private static Tasks tasks;

    def "default config is loaded"() {
        when:
        List<String> test = UserContext.getCurrentScope().getDefaultScopeConfigFiles()
        then:
        test.size() >= 1
        and:
        test.indexOf("test") >= 0
    }

    def "default config is read"() {
        when:
        String value = UserContext.getSettings().getString("settings.test")
        then:
        value == "Hello"
    }

    def "default config original contents are available"() {
        when:
        String value = UserContext.getCurrentScope().getDefaulScopeConfigContents("test")
        then:
        value == "# Test\nsettings.test =\"Hello\""
    }

    def "friend helpers are loaded"() {
        when:
        String value = UserContext.getCurrentScope().getHelper(ExampleHelper.class).getTestValue()
        then:
        value == "test"
    }

    def "helpers are cached in a consistent state"() {
        given:
        def scope = new ScopeInfo("test", "test", "test", null, null, null)
        when:
        def future1 = tasks.executor("test").start({ ->
            scope.getHelper(ExampleHelper.class).getTestValue();
        })
        def future2 = tasks.executor("test").start({ ->
            scope.getHelper(ExampleHelper.class).getTestValue();
        })
        and:
        future1.await(Duration.ofSeconds(1))
        future2.await(Duration.ofSeconds(1))
        then:
        future1.isSuccessful()
        and:
        future2.isSuccessful()
    }
}
