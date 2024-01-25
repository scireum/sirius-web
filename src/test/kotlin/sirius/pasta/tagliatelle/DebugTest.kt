/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle

import sirius.kernel.di.std.Part
import sirius.pasta.tagliatelle.rendering.GlobalRenderContext
import sirius.pasta.tagliatelle.rendering.LocalRenderContext
import sirius.web.resources.Resources

class DebugTest extends BaseSpecification {
    @Part
    private static Tagliatelle tagliatelle

            @Part
            private static Resources resources

            @Part
            private static TestHelper test

            def "debug template inclusion"() {
        given:
        String expectedResult = resources.resolve("/templates/debug/debug-expected.html").get().getContentAsString()

        when:
        Template template = tagliatelle.resolve("/templates/debug/debug.html.pasta").get()
        GlobalRenderContext renderContext = tagliatelle.createRenderContext()
        LocalRenderContext localRenderContext = renderContext.createContext(template)
        renderContext.setDebugLevel(GlobalRenderContext.DebugLevel.TRACE)
        template.renderWithContext(localRenderContext)
        String result = renderContext.toString()

        then:
        test.basicallyEqual(result, expectedResult)
    }
}
