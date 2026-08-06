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
import sirius.pasta.noodle.compiler.SourceCodeInfo
import sirius.pasta.tagliatelle.compiler.TemplateCompilationContext
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.http.CSRFHelper
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Ensures that the Wondergem upload taglibs hand the CSRF token to the uploader.
 *
 * As every upload is posted, it is subject to CSRF validation and would be rejected with a 403 without the token.
 */
@ExtendWith(SiriusExtension::class)
class WondergemUploadTest {

    @Test
    fun `w imageUpload passes the CSRF token to the uploader`() {
        val token = csrfHelper.csrfToken

        assertTrue { render("<w:imageUpload uploadUrl=\"/test/upload\"/>").contains("CSRFToken: '$token'") }
    }

    @Test
    fun `w fileUpload passes the CSRF token to the uploader`() {
        val token = csrfHelper.csrfToken

        assertTrue { render("<w:fileUpload uploadUrl=\"/test/upload\"/>").contains("CSRFToken: '$token'") }
    }

    @Test
    fun `w imageUpload accepts any extension by default`() {
        // A quoted [] would reach the uploader as a two element extension list which rejects every file
        assertFalse { render("<w:imageUpload uploadUrl=\"/test/upload\"/>").contains("'[]'") }
    }

    private fun render(source: String): String {
        val context = TemplateCompilationContext(
                Template("test.html.pasta", null),
                SourceCodeInfo.forInlineCode(source),
                null
        )
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }

        return context.template.renderToString()
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var csrfHelper: CSRFHelper
    }
}
