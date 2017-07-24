/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.base.Charsets;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.SimpleScriptContext;
import java.io.InputStreamReader;
import java.io.Reader;

/**
 * Base class for {@link sirius.web.templates.ContentHandler} implementations which rely on JavaScript.
 */
public abstract class JavaScriptBasedContentHandler implements ContentHandler {

    /**
     * Can be used to tweak the scripting engine used. By default "js" is used to select the default implementation
     * provided by the JDK.
     */
    @ConfigValue("content.script-engine")
    protected String scriptEngine;

    @Part
    private Templates templates;

    private final ScriptEngineManager manager = new ScriptEngineManager();

    /**
     * Returns a JavaScript engine as selected by the config key <tt>content.script-engine</tt>
     *
     * @return a fully initialized <tt>ScriptEngine</tt>
     */
    protected ScriptEngine getEngine() {
        return manager.getEngineByName(scriptEngine);
    }

    /**
     * Executes the template as JavaScript code
     *
     * @param generator the generator used to obtain the parameters etc.
     * @throws java.lang.Exception re-throws all exceptions
     */
    protected void execute(Generator generator) throws Exception {
        ScriptEngine engine = getEngine();

        SimpleScriptContext ctx = new SimpleScriptContext();
        templates.createGlobalContext().forEach((k, v) -> ctx.setAttribute(k, v, ScriptContext.ENGINE_SCOPE));
        generator.getContext().applyTo(ctx);

        if (Strings.isFilled(generator.getTemplateCode())) {
            engine.eval(generator.getTemplateCode(), ctx);
        } else {
            engine.put(ScriptEngine.FILENAME, generator.getTemplateName());
            try (Reader reader = new InputStreamReader(generator.getTemplate(), Charsets.UTF_8)) {
                engine.eval(reader, ctx);
            }
        }
    }
}
