/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.kernel.BaseSpecification
import sirius.kernel.async.Tasks
import sirius.kernel.di.std.Part

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
        String value = UserContext.getCurrentScope().getDefaultScopeConfigContents("test")
        then:
        value == "# Test\nsettings.test =\"Hello\""
    }

    def "helpers are instantiated via factory/constructors"() {
        when:
        def helper1 = UserContext.getCurrentScope().getHelper(FactoryExampleHelper.class)
        def helper2 = UserContext.getCurrentScope().getHelper(ExampleHelper.class)
        def helper3 = UserContext.getCurrentScope().getHelper(AnotherExampleHelper.class)
        then: "all can be instantiated"
        helper1 != null
        helper2 != null
        helper3 != null
        and: "friends are the exact same instances and not copies of the same helpers"
        helper2.getAnotherExampleHelper() == helper3
        helper3.getExampleHelper() == helper2
    }

}
