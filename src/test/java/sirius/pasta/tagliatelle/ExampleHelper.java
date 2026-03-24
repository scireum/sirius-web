/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle;

import sirius.pasta.noodle.sandbox.NoodleSandbox;

/**
 * Provides helper methods for testing Tagliatelle templates.
 */
public class ExampleHelper {

    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getTestValue() {
        return "test";
    }
}
