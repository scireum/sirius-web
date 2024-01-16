/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import sirius.kernel.SiriusExtension
import java.util.function.Predicate
import java.util.stream.Stream
import kotlin.test.assertContains
import kotlin.test.assertEquals
import kotlin.test.assertFalse

@ExtendWith(SiriusExtension::class)
class PermissionsTest {
    @Test
    fun `applyProfiles keeps original roles`() {

        val roles = hashSetOf("A", "B", "C")

        Permissions.applyProfiles(roles)

        assertContains(roles, "A")
        assertContains(roles, "B")
        assertContains(roles, "C")
    }

    @Test
    fun `applyProfiles expands known profiles`() {

        val roles = hashSetOf("test-profile")

        Permissions.applyProfiles(roles)

        assertContains(roles, "test-A")
        assertFalse { roles.contains("test-b") }
    }

    @Test
    fun `applyProfiles respects given exclusions profiles`() {

        val roles = hashSetOf("test-profile", "test-excluded-profile")
        val excludedRoles = hashSetOf("test-excluded-profile")

        Permissions.applyProfiles(roles, excludedRoles)
        excludedRoles.forEach { role -> roles.remove(role) }

        assertFalse { roles.contains("test-excluded") }

        assertFalse { roles.contains("test-excluded-profile") }

        assertContains(roles, "test-A")
        assertFalse { roles.contains("test-B") }
    }

    @ParameterizedTest
    @MethodSource("provideParameters")
    fun `test hasPermission`(
        permissionExpression: String,
        hasSinglePermissionPredicate: ((String)->Boolean)?,
        expectedResult: Boolean
    ) {
        assertEquals(expectedResult, Permissions.hasPermission(permissionExpression, hasSinglePermissionPredicate))
    }

    companion object{
        @JvmStatic
        fun provideParameters(): Stream<Arguments> {
            return Stream.of(
                Arguments.of("a",{ permission : String -> listOf("a", "b", "c").contains(permission) },true),
                Arguments.of("!a",{ permission : String -> listOf("a", "b", "c").contains(permission) },false),
                Arguments.of("d",{ permission : String -> listOf("a", "b", "c").contains(permission) } ,false),
                Arguments.of("!d",{ permission : String -> listOf("a", "b", "c").contains(permission) },true),
                Arguments.of("a+c",{ permission : String -> listOf("a", "b", "c").contains(permission) } ,true),
                Arguments.of("a+d",{ permission : String -> listOf("a", "b", "c").contains(permission) } ,false),
                Arguments.of("a,d",{ permission : String -> listOf("a", "b", "c").contains(permission) } ,true),
                Arguments.of("d,e",{ permission : String -> listOf("a", "b", "c").contains(permission) } ,false),
                Arguments.of("a",null,false),
                Arguments.of("!a",null,true),
                Arguments.of("enabled",null,true),
                Arguments.of("!enabled",null,false),
                Arguments.of("enabled+a",{ permission : String -> "a"==permission } ,true)
            )
        }
    }
}
