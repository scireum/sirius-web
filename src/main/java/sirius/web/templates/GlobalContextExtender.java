/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import java.util.function.BiConsumer;

/**
 * Can be supplied to the content model (using the {@link sirius.kernel.di.std.Register} annotation) in order to
 * extend (initialize) the contexts used by the {@link Generator}. TODO!!!!
 */
public interface GlobalContextExtender {

    /**
     * Collects the names and values of the globals.
     *
     * @param globalParameterCollector the collector to be supplied with the variable name and value.
     */
    void collect(BiConsumer<String, Object> globalParameterCollector);

}
