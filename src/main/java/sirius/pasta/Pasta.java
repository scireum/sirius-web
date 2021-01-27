/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta;

import sirius.kernel.health.Log;

/**
 * Provides a central class for common constants and utilities used by the pasta framework.
 */
public class Pasta {

    /**
     * Provides shared logger to be used by all parts of the scripting frameworks.
     */
    public static final Log LOG = Log.get("pasta");

    private Pasta() {
        // This class only provides constants, therefore the constructor is hidden...
    }
}
