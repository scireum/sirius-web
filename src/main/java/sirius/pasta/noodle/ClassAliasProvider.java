/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import java.util.function.BiConsumer;

/**
 * Provides a mapping of aliases to real java classes.
 * <p>
 * Implementations can be {@link sirius.kernel.di.std.Register registered} and then provide aliases for commonly used
 * classes.
 */
public interface ClassAliasProvider {
    /**
     * Collects all available aliases and their target classes
     *
     * @param consumer the consumer to supply with aliases
     */
    void collectAliases(BiConsumer<String, Class<?>> consumer);
}
