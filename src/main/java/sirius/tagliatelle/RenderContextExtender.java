/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Provides globally available variables.
 * <p>
 * Implementations can be {@link sirius.kernel.di.std.Register registered} and then provide global variables visible to
 * all templates.
 * <p>
 * <b>Be very careful</b> which objects are made accessible here, as through customizations, these can be used by users
 * at their own will. Therefore no inner APIs should be made accessible.
 */
public interface RenderContextExtender {

    /**
     * Collects the names and types of the golbals at compile time.
     *
     * @param parameterCollector the collector to be supplied with the variable name and type.
     */
    void collectParameterTypes(BiConsumer<String, Class<?>> parameterCollector);

    /**
     * Collects the values at runtime.
     * <p>
     * Note that the order of the supplied variables must match those in {@link #collectParameterTypes(BiConsumer)}.
     *
     * @param parameterCollector the collector to be supplied with the variable values.
     */
    void collectParameterValues(Consumer<Object> parameterCollector);
}
