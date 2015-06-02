/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import sirius.kernel.commons.Tuple;

import java.util.function.Consumer;

/**
 * Defines an extension which provides auto-declared variables to Rythm templates.
 */
public interface RythmExtension {

    /**
     * Enumerates all variables paired with their according type.
     * <p>
     * This method is used at compile-time, to perform type checks.
     *
     * @param names a collector which can be used to supply variables along with their type.
     */
    void collectExtensionNames(Consumer<Tuple<String, Class<?>>> names);

    /**
     * Enumerates all variables paired with their according value.
     * <p>
     * This method is used at runtime, to supply the appropriate values.
     *
     * @param values a collector which can be used to supply variables along with their value.
     */
    void collectExtensionValues(Consumer<Tuple<String, Object>> values);
}
