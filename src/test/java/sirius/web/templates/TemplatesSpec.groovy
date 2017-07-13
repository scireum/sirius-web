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
                .direct('@hello', TagliatelleContentHandler.VM)
                .generate()
        then:
        result == "World"
    }

    def "template lookup works"() {
        when:
        def result = templates.generator().useTemplate("helloWorld.pasta")
                .applyContext(Context.create().set("hello", "World"))
                .generate()
        then:
        result == "Hello World"
    }

    def "compund template names work"() {
        when:
        def result = templates.generator().useTemplate("helloWorld.js.pasta")
                .applyContext(Context.create().set("hello", "World"))
                .generate()
        then:
        result == "var text = 'World';"
    }

    def "javascript execution works"() {
        when:
        def result = templates.generator().useTemplate("template.js")
                .applyContext(Context.create().set("x", "5"))
                .generate()
        then:
        result == "25.0"
    }

    def "javascript generating XML works"() {
        when:
        def result = templates.generator().useTemplate("template.xml.js")
                .applyContext(Context.create().set("x", "5"))
                .generate()
        then:
        result == '<?xml version="1.0" encoding="UTF-8"?><result>\n<x>5</x>\n</result>\n'
    }

}
