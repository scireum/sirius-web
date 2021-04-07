/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

/**
 * Represents another generic test class.
 *
 * @param <T> the generic type of the test member
 */
public abstract class NoodleTestBase<T> {

    protected T test;

    public T getTest() {
        return test;
    }
}
