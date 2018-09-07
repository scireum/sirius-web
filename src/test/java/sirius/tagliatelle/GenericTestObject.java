/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.commons.Amount;

public class GenericTestObject<X> {

    public X genericTest(X input) {
        return input;
    }

    public Amount genericTest(Amount x) {
        return x.times(Amount.MINUS_ONE);
    }
}
