/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.pasta.tagliatelle.rendering.GlobalRenderContext
import sirius.web.resources.Resources
import kotlin.test.assertTrue

/**
 * Tests the debug output of the [Tagliatelle] template engine.
 */
@ExtendWith(SiriusExtension::class)
class DebugTest {

    @Test
    fun `Debug information is included in template when debug level is set to TRACE`() {
        val expectedResult = resources.resolve("/templates/debug/debug-expected.html").get().contentAsString

        val template = tagliatelle.resolve("/templates/debug/debug.html.pasta").get()
        val renderContext = tagliatelle.createRenderContext()
        val localRenderContext = renderContext.createContext(template)
        renderContext.debugLevel = GlobalRenderContext.DebugLevel.TRACE
        template.renderWithContext(localRenderContext)
        val result = renderContext.toString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var tagliatelle: Tagliatelle

        @JvmStatic
        @Part
        private lateinit var resources: Resources

        @JvmStatic
        @Part
        private lateinit var test: TestHelper
    }
}
