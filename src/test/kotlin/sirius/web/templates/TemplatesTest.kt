/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Context
import sirius.kernel.di.std.Part
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class TemplatesTest {

    companion object {
        @Part
        @JvmStatic
        private lateinit var templates: Templates
    }

    @Test
    fun `direct generation works`() {

        val result = templates.generator()
            .applyContext(Context.create().set("hello", "World"))
            .direct("<i:arg type=\"String\" name=\"hello\" />@hello", TagliatelleContentHandler.PASTA)
            .generate()

        assertEquals("World", result)
    }

    @Test
    fun `template lookup works`() {

        val result = templates.generator().useTemplate("/templates/helloWorld.pasta")
            .applyContext(Context.create().set("hello", "World"))
            .generate()

        assertEquals("Hello World", result)
    }

    @Test
    fun `compound template names work`() {

        val result = templates.generator().useTemplate("/templates/helloWorld.js.pasta")
            .applyContext(Context.create().set("hello", "World"))
            .generate()

        assertEquals("var text = 'World';", result)
    }

}
