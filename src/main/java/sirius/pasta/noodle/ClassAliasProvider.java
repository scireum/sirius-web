/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import sirius.kernel.di.std.Priorized;

import java.util.function.BiConsumer;

/**
 * Provides a mapping of aliases to real java classes.
 * <p>
 * Implementations can be {@link sirius.kernel.di.std.Register registered} and then provide aliases for commonly used
 * classes.
 */
public interface ClassAliasProvider extends Priorized {
    /**
     * Collects all available aliases and their target classes
     *
     * @param consumer the consumer to supply with aliases
     */
    void collectAliases(BiConsumer<String, Class<?>> consumer);

    /**
     * Permits to provide optional aliases which should only be introduced if no conflicting alias exists.
     *
     * @param consumer the consumer to supply with aliases
     */
    default void collectOptionalAliases(BiConsumer<String, Class<?>> consumer) {
    }

}
