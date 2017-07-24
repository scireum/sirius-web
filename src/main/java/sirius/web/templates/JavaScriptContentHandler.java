/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.di.std.Register;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Executes the given JavaScript script (most probably without generating any output).
 * <p>
 * This handler expects JavaScript as template language. The name of this handler is <b>js</b> the expected file
 * extension is
 * <b>.js</b>
 */
@Register(name = JavaScriptContentHandler.JS, classes = ContentHandler.class)
public class JavaScriptContentHandler extends JavaScriptBasedContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String JS = "js";

    @Override
    public boolean generate(Generator generator, OutputStream out) throws Exception {
        if (!JS.equals(generator.getHandlerType()) && !generator.isTemplateFileExtension("js")) {
            return false;
        }

        PrintWriter writer = new PrintWriter(out == null ?
                                             new StringWriter() :
                                             new OutputStreamWriter(out, generator.getEncoding()));
        generator.put("out", writer);
        execute(generator);
        writer.flush();

        return true;
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }
}
