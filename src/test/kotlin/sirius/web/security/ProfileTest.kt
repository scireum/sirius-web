/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertDoesNotThrow
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.Sirius
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Strings
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class ProfileTest {

    @Test
    fun `profile cascading is invalid when cascading to lower priority`() {

        try {
            Profile.compile(
                Sirius.getSettings().getExtension(
                    "security.profiles",
                    "test-cascade-to-target-with-lower-priority"
                )
            )
                .validate()
        } catch (exception: IllegalStateException) {
            assertEquals(
                createErrorMessage("test-cascade-to-target-with-lower-priority", "cascade-target"),
                exception.message
            )
        }
    }

    @Test
    fun `profile cascading is invalid when cascading to equal priority`() {
        try {
            Profile.compile(
                Sirius.getSettings().getExtension(
                    "security.profiles",
                    "test-cascade-to-target-with-equal-priority"
                )
            )
                .validate()
        } catch (exception: IllegalStateException) {
            assertEquals(
                createErrorMessage("test-cascade-to-target-with-equal-priority", "cascade-target"),
                exception.message
            )
        }
    }

    @Test
    fun `profile cascading is valid when cascading to higher priority`() {
        assertDoesNotThrow {
            Profile.compile(
                Sirius.getSettings().getExtension(
                    "security.profiles",
                    "test-cascade-to-target-with-higher-priority"
                )
            )
                .validate()
        }
    }

    private fun createErrorMessage(profile1: String, profile2: String): String {
        return Strings.apply(
            "Profile '%s' refers to a profile which is applied " +
                    "earlier than itself ('%s'). Therefore the profiles will not be resolved completely. Fix " +
                    "this by adding or changing priorities.", profile1, profile2
        )
    }
}
