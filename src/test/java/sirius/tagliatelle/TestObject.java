/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.ValueHolder;

import java.util.function.Function;

/**
 * Used to test various scenarios regarding generics etc.
 */
public class TestObject extends GenericTestObject<String> {

    public static final TestObject INSTANCE = new TestObject();

    public Tuple<Function<String, ValueHolder<String>>, Integer> getGenericReturnType() {
        return Tuple.create(s -> new ValueHolder<>(s), 1);
    }

    public String varArgTest(String input, Object... params) {
        return Strings.apply(input, params);
    }

    @Deprecated
    public void deprecatedMethod() {
        // Nothing to do...
    }
}
