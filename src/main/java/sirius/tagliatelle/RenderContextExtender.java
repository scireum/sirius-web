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
 * Created by aha on 15.05.17.
 */
public interface RenderContextExtender {

    void collectParameterTypes(BiConsumer<String, Class<?>> parameterCollector);

    void collectParameterValues(Consumer<Object> parameterCollector);

}
