/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros

import sirius.kernel.BaseSpecification
import sirius.kernel.di.std.Part
import sirius.pasta.tagliatelle.Tagliatelle
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources

class Base64ResourceMacroSpec extends BaseSpecification {

    @Part
    private static Tagliatelle tagliatelle

    @Part
    private static Resources resources

    def "base64Resource inlines the encoded file with correct media type"() {
        when:
        def ctx = tagliatelle.createInlineCompilationContext("inline", "@base64Resource('/assets/test.png')", null)
        new TemplateCompiler(ctx).compile()
        then:
        ctx
                .getTemplate()
                .renderToString() == "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADIAAAAyCAIAAACRXR/mAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4QsODw4S4KU/XgAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAAdElEQVRYw+3Q3Q1AMAAA4ar+qHhghq7SBQ3CPsQO5a1JS0wh+nA3wZdr5hBEfUlRZbBgwYIFCxYsWLBgwYIFCxYsWLBgwYIFCxYsWLBgfZSqDRRtWv1eC2vx2zFGV7S5pX2U+n3M2SWX1ZCNv6a+aHNL/bQvbxUXkThEKBQAAAAASUVORK5CYII="
    }
}
