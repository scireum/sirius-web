/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

/**
 * An implementation of the generic test class.
 */
public class NoodleTestRef extends NoodleTestRefBase<Long> {

    public NoodleTestRef() {
        test = "Hello";
        id = 42L;
    }
}
