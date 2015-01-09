package sirius.web.templates;

import com.google.common.base.Charsets;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.xml.XMLStructuredOutput;

import javax.script.ScriptEngine;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;

/**
 * Generates XML output by evaluating a given JavaScript.
 * <p>
 * This handler expects JavaScript as template language and passes a special variable "xml" of type
 * {@link XMLStructuredOutput} in. The name of this handler is <b>xml-js</b> the expected file extension is
 * <b>.xml.js</b>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/02
 */
@Register(name = JsXMLContentHandler.XML_JS, classes = ContentHandler.class)
public class JsXMLContentHandler extends JavaScriptContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String XML_JS = "xml-js";

    @Override
    public boolean generate(Content.Generator generator, OutputStream out) throws Exception {
        if (!XML_JS.equals(generator.getHandlerType()) && !generator.isTemplateEndsWith(".xml.js")) {
            return false;
        }

        XMLStructuredOutput xmlOut = new XMLStructuredOutput(out);
        generator.getContext().put("xml", xmlOut);
        xmlOut.beginResult();
        execute(generator);
        xmlOut.endResult();

        return true;
    }

    /*
     * Evaluates the given template or script as JavaScript
     */
    private void execute(Content.Generator generator) throws Exception {
        ScriptEngine engine = getEngine();
        ScriptingContext ctx = new ScriptingContext();
        generator.getContext().applyTo(ctx);
        if (Strings.isFilled(generator.getTemplateCode())) {
            engine.eval(generator.getTemplateCode(), ctx);
        } else {
            engine.put(ScriptEngine.FILENAME, generator.getTemplateName());
            Reader reader = new InputStreamReader(generator.getTemplate(), Charsets.UTF_8);
            try {
                engine.eval(reader, ctx);
            } finally {
                reader.close();
            }
        }
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }
}
