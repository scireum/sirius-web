/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.kernel.BaseSpecification

import java.util.function.Predicate

class PermissionsSpec extends BaseSpecification {

    def "applyProfiles keeps original roles"() {
        when:
        def roles = new HashSet<String>(["A", "B", "C"])
        and:
        Permissions.applyProfiles(roles)
        then:
        roles.contains("A")
        and:
        roles.contains("B")
        and:
        roles.contains("C")
    }

    def "applyProfiles expands known profiles"() {
        when:
        def roles = new HashSet<String>(["test-profile"])
        and:
        Permissions.applyProfiles(roles)
        then:
        roles.contains("test-A")
        and:
        !roles.contains("test-B")
    }

    def "applyProfiles respects given exclusions profiles"() {
        when:
        def roles = new HashSet<String>(["test-profile", "test-excluded-profile"])
        def excludedRoles = new HashSet<String>(["test-excluded-profile"])
        and:
        Permissions.applyProfiles(roles, excludedRoles)
        excludedRoles.forEach { role -> roles.remove(role) }
        then:
        !roles.contains("test-excluded")
        !roles.contains("test-excluded-profile")
        and:
        roles.contains("test-A")
        !roles.contains("test-B")
    }

    def "test hasPermission"() {
        expect:
        Permissions.hasPermission(permissionExpression, hasSinglePermissionPredicate) == expectedResult
        where:
        permissionExpression | hasSinglePermissionPredicate                                           | expectedResult
        "a"                  | { permission -> ["a", "b", "c"].contains(permission) } as Predicate    | true
        "!a"                 | { permission -> ["a", "b", "c"].contains(permission) } as Predicate    | false
        "d"                  | { permission -> ["a", "b", "c"].contains(permission) } as Predicate    | false
        "!d"                 | { permission -> ["a", "b", "c"].contains(permission) } as Predicate    | true
        "a+c"                | { permission -> ["a", "b", "c"].contains(permission) } as Predicate | true
        "a+d"                | { permission -> ["a", "b", "c"].contains(permission) } as Predicate | false
        "a,d"                | { permission -> ["a", "b", "c"].contains(permission) } as Predicate | true
        "d,e"                | { permission -> ["a", "b", "c"].contains(permission) } as Predicate | false
        "a"                  | null                                                                   | false
        "!a"                 | null                                                                   | true
        "enabled"            | null                                                                   | true
        "!enabled"           | null                                                                   | false
        "enabled+a"          | { permission -> ("a" == permission) }                                  | true
    }
}
