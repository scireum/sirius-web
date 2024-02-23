/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.web.sass.Output
import java.io.StringWriter
import kotlin.test.assertTrue

/**
 * Tests rendering of application scss.
 */
@ExtendWith(SiriusExtension::class)
class ApplicationSCSSTest {
    @Test
    fun `application scss can be compiled`() {
        val generator = TestGenerator()
        generator.importStylesheet("/assets/wondergem/stylesheets/application.scss")
        generator.compile()
        // Let the content compressor take care of minifying the CSS
        val writer = StringWriter()
        val output = Output(writer, false)
        generator.generate(output)
        writer.close()
        assertTrue { writer.toString().length > 0 }
    }
}
