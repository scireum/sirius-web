/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.kernel.BaseSpecification
import sirius.kernel.Sirius

class ProfileTest extends BaseSpecification {

    def "profile cascading is invalid when cascading to lower priority"() {
        when:
        Profile.compile(Sirius.getSettings().getExtension("security.profiles",
                                                          "test-cascade-to-target-with-lower-priority"))
               .validate()
        then:
        def e = thrown(IllegalStateException)
        e.getMessage() == "Profile 'test-cascade-to-target-with-lower-priority' refers to a profile which is applied " +
                "earlier than itself ('cascade-target'). Therefore the profiles will not be resolved completely. Fix " +
                "this by adding priorities."
    }

    def "profile cascading is invalid when cascading to equal priority"() {
        when:
        Profile.compile(Sirius.getSettings().getExtension("security.profiles",
                                                          "test-cascade-to-target-with-equal-priority"))
               .validate()
        then:
        def e = thrown(IllegalStateException)
        e.getMessage() == "Profile 'test-cascade-to-target-with-equal-priority' refers to a profile which is applied " +
                "earlier than itself ('cascade-target'). Therefore the profiles will not be resolved completely. Fix " +
                "this by adding priorities."
    }

    def "profile cascading is valid when cascading to higher priority"() {
        when:
        Profile.compile(Sirius.getSettings().getExtension("security.profiles",
                                                          "test-cascade-to-target-with-higher-priority"))
               .validate()
        then:
        noExceptionThrown()
    }
}
