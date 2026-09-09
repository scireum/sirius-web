/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;

@Register(classes = TestHelper.class)
public class TestHelper {
    public boolean basicallyEqual(String left, String right) {
        return Strings.areEqual(left.replaceAll("\\s", ""), right.replaceAll("\\s", ""));
    }
}
