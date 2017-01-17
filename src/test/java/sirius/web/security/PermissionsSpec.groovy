/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.kernel.BaseSpecification

class PermissionsSpec extends BaseSpecification {

    def "applyProfiles keeps original roles"() {
        when:
        def roles = Permissions.applyProfiles(["A", "B", "C"])
        then:
        roles.contains("A")
        and:
        roles.contains("B")
        and:
        roles.contains("C")
    }

    def "applyProfiles expands known profiles"() {
        when:
        def roles = Permissions.applyProfiles(["test-profile"])
        then:
        roles.contains("test-A")
        and:
        !roles.contains("test-B")
    }

}
