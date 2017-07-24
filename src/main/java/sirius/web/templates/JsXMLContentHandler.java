/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.di.std.Register;
import sirius.kernel.xml.XMLStructuredOutput;

import java.io.OutputStream;

/**
 * Generates XML output by evaluating a given JavaScript.
 * <p>
 * This handler expects JavaScript as template language and passes a special variable "xml" of type
 * {@link XMLStructuredOutput} in. The name of this handler is <b>xml-js</b> the expected file extension is
 * <b>.xml.js</b>
 */
@Register(name = JsXMLContentHandler.XML_JS, classes = ContentHandler.class)
public class JsXMLContentHandler extends JavaScriptBasedContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String XML_JS = "xml-js";

    @Override
    public boolean generate(Generator generator, OutputStream out) throws Exception {
        if (!XML_JS.equals(generator.getHandlerType()) && !generator.isTemplateFileExtension("xml.js")) {
            return false;
        }

        XMLStructuredOutput xmlOut = new XMLStructuredOutput(out);
        generator.getContext().put("xml", xmlOut);
        xmlOut.beginResult();
        execute(generator);
        xmlOut.endResult();

        return true;
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }
}
