/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle

import sirius.kernel.BaseSpecification
import sirius.kernel.di.std.Part
/**
 * Created by aha on 11.05.17.
 */
class CompilerSpec extends BaseSpecification {


    @Part
    private static Tagliatelle engine

    def "parser works"() {
        when:
        Template t = engine.resolve("test.html.pasta").get()
        and:
        String result = t.renderToString()
        then:
        result == "<p>\n<h1>Test</h1><p>Hello World</p></p>"

    }

}
