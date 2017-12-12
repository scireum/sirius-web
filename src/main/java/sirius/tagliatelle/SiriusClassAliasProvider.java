/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Register;
import sirius.web.controller.Page;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;

/**
 * Provides basic aliases for standard Java classes.
 */
@Register
public class SiriusClassAliasProvider implements ClassAliasProvider {

    @Override
    public void collectAliases(BiConsumer<String, Class<?>> consumer) {
        consumer.accept("String", String.class);
        consumer.accept("int", int.class);
        consumer.accept("boolean", boolean.class);
        consumer.accept("List", List.class);
        consumer.accept("Page", Page.class);
        consumer.accept("Tuple", Tuple.class);
        consumer.accept("Map", Map.class);
        consumer.accept("Set", Set.class);
        consumer.accept("Object", Object.class);
    }
}
