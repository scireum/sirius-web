/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import sirius.pasta.noodle.sandbox.NoodleSandbox;

/**
 * Represents another generic test class.
 *
 * @param <T> the generic type of the test member
 */
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public abstract class NoodleTestBase<T> {

    protected T test;

    public T getTest() {
        return test;
    }
}
