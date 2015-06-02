/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import org.apache.velocity.context.Context;

import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

/**
 * Simple adapter for {@link ScriptContext} (javax.scripting) and {@link Context} (Velocity).
 */
public class ScriptingContext extends SimpleScriptContext implements Context {

    @Override
    public Object put(String key, Object value) {
        Object old = getAttribute(key, ScriptContext.ENGINE_SCOPE);
        setAttribute(key, value, ScriptContext.ENGINE_SCOPE);
        return old;
    }

    @Override
    public Object get(String key) {
        return getAttribute(key, ScriptContext.ENGINE_SCOPE);
    }

    @Override
    public boolean containsKey(Object key) {
        return getBindings(ENGINE_SCOPE).containsKey(key);
    }

    @Override
    public Object[] getKeys() {
        return getBindings(GLOBAL_SCOPE).keySet().toArray(new Object[getBindings(GLOBAL_SCOPE).keySet().size()]);
    }

    @Override
    public Object remove(Object key) {
        return getBindings(ENGINE_SCOPE).remove(key);
    }
}
