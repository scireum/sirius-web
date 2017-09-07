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
import sirius.web.resources.Resources

class CompilerSpec extends BaseSpecification {

    @Part
    private static Tagliatelle tagliatelle
        @Part
    private static Resources resources

    def "mixing of { brakets works as expected"() {
        when:
        String result = tagliatelle.resolve("templates/brakets.html.pasta").get().renderToString()
        then:
        result == resources.resolve("templates/brakets.html").get().getContentAsString()
    }

}
