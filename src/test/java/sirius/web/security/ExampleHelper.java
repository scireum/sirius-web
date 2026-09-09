/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

public class ExampleHelper {

    @Helper
    private AnotherExampleHelper anotherExampleHelper;

    public AnotherExampleHelper getAnotherExampleHelper() {
        return anotherExampleHelper;
    }
}
