/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates

import sirius.kernel.BaseSpecification
import sirius.kernel.commons.Context
import sirius.kernel.di.std.Part

class TemplatesSpec extends BaseSpecification {

    @Part
    private static Templates templates

    def "direct generation works"() {
        when:
        def result = templates.generator()
                .applyContext(Context.create().set("hello", "World"))
                .direct('<i:arg type="String" name="hello" />@hello', TagliatelleContentHandler.PASTA)
                .generate()
        then:
        result == "World"
    }

    def "template lookup works"() {
        when:
        def result = templates.generator().useTemplate("/templates/helloWorld.pasta")
                .applyContext(Context.create().set("hello", "World"))
                .generate()
        then:
        result == "Hello World"
    }

    def "compund template names work"() {
        when:
        def result = templates.generator().useTemplate("/templates/helloWorld.js.pasta")
                .applyContext(Context.create().set("hello", "World"))
                .generate()
        then:
        result == "var text = 'World';"
    }

}
