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
import sirius.kernel.SiriusExtension
import kotlin.test.assertEquals
import kotlin.test.assertTrue

@ExtendWith(SiriusExtension::class)
class ScopeInfoTest {

    @Test
    fun `default config is loaded`() {

        val test = ScopeInfo.getDefaultScopeConfigFiles()

        assertTrue { test.size >= 1 }
        assertTrue { test.indexOf("test") >= 0 }
    }

    @Test
    fun `default config is read`() {

        val value = UserContext.getSettings().getString("settings.test")

        assertEquals("Hello", value)
    }

    @Test
    fun `default config original contents are available`() {

        val value = ScopeInfo.getDefaultScopeConfigContents("test")

        assertEquals("# Test\nsettings.test =\"Hello\"", value)
    }

    @Test
    fun `helpers are instantiated via factory or constructors`() {

        val helper1 = UserContext.getCurrentScope().getHelper(FactoryExampleHelper::class.java)
        val helper2 = UserContext.getCurrentScope().getHelper(ExampleHelper::class.java)
        val helper3 = UserContext.getCurrentScope().getHelper(AnotherExampleHelper::class.java)

        helper1 != null
        helper2 != null
        helper3 != null

        assertEquals(helper3, helper2.anotherExampleHelper)
        assertEquals(helper2, helper3.exampleHelper)
    }

}
