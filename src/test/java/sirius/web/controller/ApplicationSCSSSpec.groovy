/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller

import org.serversass.Generator
import org.serversass.Output
import org.serversass.ast.Value
import sirius.kernel.BaseSpecification
import sirius.kernel.di.std.Part
import sirius.web.http.WebContext
import sirius.web.templates.Resource
import sirius.web.templates.Resources

class ApplicationSCSSSpec extends BaseSpecification {

    class TestGenerator extends Generator {
        @Override
         void debug(String message) {

        }

        @Override
         void warn(String message) {
            throw new RuntimeException(message)
        }

        @Part
        private static Resources resources

        @Override
        protected InputStream resolveIntoStream(String sheet) throws IOException {
            Optional<Resource> res = resources.resolve(sheet)
            if (res.isPresent()) {
                return res.get().getUrl().openStream()
            }
            return null
        }

        TestGenerator() {
            scope.set("prefix", new Value(WebContext.getContextPrefix()))
        }
    }

    def "application.scss can be compiled"() {
        when:
        TestGenerator gen = new TestGenerator()
        gen.importStylesheet("/assets/wondergem/stylesheets/application.scss")
        gen.compile()
        // Let the content compressor take care of minifying the CSS
        StringWriter writer = new StringWriter()
        Output out = new Output(writer, false)
        gen.generate(out)
        writer.close()
        then:
        writer.toString().length() > 0
    }

}
