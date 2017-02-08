/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch

import sirius.web.http.WebContext
import spock.lang.Specification

class HelpDispatcherTest extends Specification {

    HelpDispatcher helpDispatcher;

    def setup() {
        helpDispatcher = new HelpDispatcher()
        helpDispatcher.helpSystemLanguageDirectories = ["de", "en"]
        helpDispatcher.indexTemplate = "index"
    }


    def "test getHelpSystemLanguageDirectory"() {
        expect:
        output == helpDispatcher.getHelpSystemLanguageDirectory(input)
        where:
        input                | output
        "/help/en"           | "en"
        "/help/en/"          | "en"
        "/help/en/test-page" | "en"
        "/help/encoding"     | ""
        "/help/de"           | "de"
        "/help/decoding"     | ""
        "/help/es"           | ""
        "/help/"             | ""
        "/help/index"        | ""
    }

    def "test getRequestedURI"() {
        expect:
        output == helpDispatcher.getRequestedURI(getWebContext(input))
        where:
        input                | output
        "/help/en"           | "/help/en/index"
        "/help/en/"          | "/help/en/index"
        "/help/en/test-page" | "/help/en/test-page"
        "/help/encoding"     | "/help/encoding"
        "/help/de"           | "/help/de/index"
        "/help/decoding"     | "/help/decoding"
        "/help/es"           | "/help/es"
        "/help/"             | "/help/index"
        "/help"              | "/help/index"
    }

    def getWebContext(input) {
        WebContext ctx = Mock(WebContext)
        ctx.getRequestedURI() >> {
            return input
        }
        return ctx;
    }
}
