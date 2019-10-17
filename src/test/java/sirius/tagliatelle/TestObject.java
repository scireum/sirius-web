/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.commons.Strings;

/**
 * Used to test vararg methods and method overloading with generics
 */
public class TestObject extends GenericTestObject<String> {

    public static final TestObject INSTANCE = new TestObject();

    public String varArgTest(String input, Object... params) {
        return Strings.apply(input, params);
    }

    @Deprecated
    public void deprecatedMethod() {
        // Nothing to do...
    }
}
