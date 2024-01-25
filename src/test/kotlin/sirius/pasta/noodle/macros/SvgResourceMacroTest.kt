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
import kotlin.test.assertEquals

/**
 * Tests the [SvgResourceMacro].
 */
@ExtendWith(SiriusExtension::class)
class SvgResourceMacroTest {

    @Test
    fun `SvgResource without tint color loads the black, minified version`() {
        val context = tagliatelle.createInlineCompilationContext("inline", "@svgResource('/assets/test.svg')", null)
        TemplateCompiler(context).compile()
        assertEquals(
                "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGZpbGw9IiMwMDAwMDAiIGhlaWdodD0iMzIiIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDMyIDMyIiB3aWR0aD0iMzIiPjxwYXRoIGQ9Im0yNiA3YzEuMTA0NiAwIDIgMC44OTU0MyAyIDJ2MTVjMCAxLjEwNDYtMC44OTU0MyAyLTIgMmgtMTljLTEuMTA0NiAwLTItMC44OTU0My0yLTJ2LTE1YzAtMS4xMDQ2IDAuODk1NDMtMiAyLTJoMTl6bTAgMmgtMTl2MTVoMTl2LTE1em0tMTIuMiAzYzAuMTcwNCAwIDAuMzIgMC4wNjE1IDAuNDUwNCAwLjE0N2w2LjIxNjggMy42NTRjMC4zMDg4IDAuMTAyNzUgMC41MzI4IDAuMzczNSAwLjUzMjggMC42OTkgMCAwLjI0Mzc1LTAuMTMyIDAuNDUwNzUtMC4zMjQgMC41ODcyNWwtNi4zNzkyIDMuNzM2NWMtMC4xMzc2IDAuMTA0MjUtMC4zMDY0IDAuMTc2MjUtMC40OTY4IDAuMTc2MjUtMC40NDI0IDAtMC44LTAuMzM1MjUtMC44LTAuNzV2LTcuNWMwLTAuNDE0IDAuMzU3Ni0wLjc1IDAuOC0wLjc1eiIvPjwvc3ZnPg==",
                context.template.renderToString()
        )
    }

    @Test
    fun `SvgResource with tint color loads a colored, minified version`() {
        val context = tagliatelle.createInlineCompilationContext(
                "inline",
                "@svgResource('/assets/test.svg', '#ff0000')",
                null
        )
        TemplateCompiler(context).compile()
        assertEquals(
                "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGZpbGw9IiNmZjAwMDAiIGhlaWdodD0iMzIiIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDMyIDMyIiB3aWR0aD0iMzIiPjxwYXRoIGQ9Im0yNiA3YzEuMTA0NiAwIDIgMC44OTU0MyAyIDJ2MTVjMCAxLjEwNDYtMC44OTU0MyAyLTIgMmgtMTljLTEuMTA0NiAwLTItMC44OTU0My0yLTJ2LTE1YzAtMS4xMDQ2IDAuODk1NDMtMiAyLTJoMTl6bTAgMmgtMTl2MTVoMTl2LTE1em0tMTIuMiAzYzAuMTcwNCAwIDAuMzIgMC4wNjE1IDAuNDUwNCAwLjE0N2w2LjIxNjggMy42NTRjMC4zMDg4IDAuMTAyNzUgMC41MzI4IDAuMzczNSAwLjUzMjggMC42OTkgMCAwLjI0Mzc1LTAuMTMyIDAuNDUwNzUtMC4zMjQgMC41ODcyNWwtNi4zNzkyIDMuNzM2NWMtMC4xMzc2IDAuMTA0MjUtMC4zMDY0IDAuMTc2MjUtMC40OTY4IDAuMTc2MjUtMC40NDI0IDAtMC44LTAuMzM1MjUtMC44LTAuNzV2LTcuNWMwLTAuNDE0IDAuMzU3Ni0wLjc1IDAuOC0wLjc1eiIvPjwvc3ZnPg==",
                context.template.renderToString()
        )
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var tagliatelle: Tagliatelle
    }
}
