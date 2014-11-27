/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates

import com.google.common.io.ByteStreams
import sirius.kernel.BaseSpecification
import sirius.kernel.di.Injector

class ODTPDFContentHandlerSpecXXX extends BaseSpecification {

    def "reading a ODT file"() {
        given:
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        Injector.context().getPart(Content.class).generator()
                .put("list", Arrays.asList("X", "Y", "Z"))
                .put("list1", null)
                .useTemplate("/test.pdf.odt")
                .generateTo(buffer);
        when:
        ByteStreams.copy(new ByteArrayInputStream(buffer.toByteArray()), new FileOutputStream("output.odt"));
        then:
        new File("output.pdf").exists()
    }

}
