/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.di.std.ConfigValue;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

/**
 * Base class for {@link sirius.web.templates.ContentHandler} implementations which rely on JavaScript.
 *
 * @author Andreas Haufler (aha@‚cireum.de)
 * @since 2014/11
 */
public abstract class JavaScriptContentHandler implements ContentHandler {

    /**
     * Can be used to tweak the scripting engine used. By default "js" is used to select the default implementation
     * provided by the JDK. As Java 8 will probably support a faster engine (Nashorn) with JIT to Java and therefore
     * eventually to machine code, we load this setting from the system configuration.
     */
    @ConfigValue("content.script-engine")
    protected String scriptEngine;

    private final ScriptEngineManager manager = new ScriptEngineManager();

    /**
     * Returns a JavaScript engine as selected by the config key <tt>content.script-engine</tt>
     *
     * @return a fully initialized <tt>ScriptEngine</tt>
     */
    protected ScriptEngine getEngine() {
        return manager.getEngineByName(scriptEngine);
    }

}
