/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import sirius.kernel.nls.NLS;

import java.lang.reflect.Type;

/**
 * Represents an optimized call which simply performs {@link NLS#get(String)}.
 */
public class NLSCall implements Callable {

    private final String key;

    /**
     * Creates a new instance which translates the given key.
     *
     * @param key the translation key to fetch
     */
    public NLSCall(String key) {
        this.key = key;
    }

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public Type getGenericType() {
        return getType();
    }

    @Override
    public Object call(Environment environment) {
        return NLS.get(key);
    }

    @Override
    public String toString() {
        return "NLSCall: " + key;
    }
}
