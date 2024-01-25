/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.pasta.tagliatelle.Tagliatelle
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources
import kotlin.test.assertEquals

/**
 * Tests the [Base64ResourceMacro].
 */
@ExtendWith(SiriusExtension::class)
class Base64ResourceMacroTest {

    @Test
    fun `Base64Resource inlines the encoded file with correct media type`() {
        val context = tagliatelle.createInlineCompilationContext("inline", "@base64Resource('/assets/test.png')", null)
        TemplateCompiler(context).compile()

        assertEquals(
                "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADIAAAAyCAIAAACRXR/mAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4QsODw4S4KU/XgAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAAdElEQVRYw+3Q3Q1AMAAA4ar+qHhghq7SBQ3CPsQO5a1JS0wh+nA3wZdr5hBEfUlRZbBgwYIFCxYsWLBgwYIFCxYsWLBgwYIFCxYsWLBgfZSqDRRtWv1eC2vx2zFGV7S5pX2U+n3M2SWX1ZCNv6a+aHNL/bQvbxUXkThEKBQAAAAASUVORK5CYII=",
                context.template.renderToString()
        )
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var tagliatelle: Tagliatelle

        @JvmStatic
        @Part
        private lateinit var resources: Resources
    }
}
