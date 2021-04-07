/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

/**
 * Represents a generic test class.
 *
 * @param <T> the generic type of the test member
 */
public abstract class NoodleTestRefBase<T> extends NoodleTestBase<String> {

    protected T id;

    public T getId() {
        return id;
    }
}
