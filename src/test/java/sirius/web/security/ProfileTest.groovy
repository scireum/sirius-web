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
import sirius.kernel.commons.Strings

class ProfileTest extends BaseSpecification {

    def "profile cascading is invalid when cascading to lower priority"() {
        when:
        Profile.compile(Sirius.getSettings().getExtension("security.profiles",
                                                          "test-cascade-to-target-with-lower-priority"))
               .validate()
        then:
        def e = thrown(IllegalStateException)
        e.getMessage() == createErrorMessage("test-cascade-to-target-with-lower-priority", "cascade-target")
    }

    def "profile cascading is invalid when cascading to equal priority"() {
        when:
        Profile.compile(Sirius.getSettings().getExtension("security.profiles",
                                                          "test-cascade-to-target-with-equal-priority"))
               .validate()
        then:
        def e = thrown(IllegalStateException)
        e.getMessage() == createErrorMessage("test-cascade-to-target-with-equal-priority", "cascade-target")
    }

    def "profile cascading is valid when cascading to higher priority"() {
        when:
        Profile.compile(Sirius.getSettings().getExtension("security.profiles",
                                                          "test-cascade-to-target-with-higher-priority"))
               .validate()
        then:
        noExceptionThrown()
    }

    def createErrorMessage(profile1, profile2) {
        return Strings.apply(
                "Profile '%s' refers to a profile which is applied " +
                        "earlier than itself ('%s'). Therefore the profiles will not be resolved completely. Fix " +
                        "this by adding or changing priorities.", profile1, profile2)
    }
}
