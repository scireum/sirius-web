/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Can be supplied to the content model (using the {@link sirius.kernel.di.std.Register} annotation) in order to
 * extend (initialize) the contexts used by the {@link Generator}.
 */
public interface GlobalContextExtender {

    /**
     * Defines the collector used to supply global variables.
     */
    interface Collector {

        /**
         * Provides the given object as variable with the given name.
         * <p>
         * Note that the type will be derived automatically therefore the object must not be null.
         *
         * @param name   the name of the global variable
         * @param object the value of the global variable
         */
        void collect(String name, @Nonnull Object object);

        /**
         * Provides the given object as variable with the given name.
         *
         * @param name   the name of the global variable
         * @param object the value of the global variable
         * @param type   the type of the global variable
         */
        void collect(String name, @Nullable Object object, Class<?> type);
    }

    /**
     * Collects the names and values of the globals avilable when generating content.
     *
     * @param globalParameterCollector the collector to be supplied with the variable name and value.
     */
    void collectTemplate(Collector globalParameterCollector);

    /**
     * Collects the names and values of globals available when executing a system script.
     *
     * @param globalParameterCollector the collector to be supplied with the variable name and value.
     */
    void collectScripting(Collector globalParameterCollector);
}
