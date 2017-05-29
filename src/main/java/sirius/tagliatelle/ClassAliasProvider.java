/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import java.util.function.BiConsumer;

/**
 * Created by aha on 23.05.17.
 */
public interface ClassAliasProvider {
    void collectAliases(BiConsumer<String, Class<?>> consumer);
}
