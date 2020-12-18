/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

public class AnotherExampleHelper {

    private final ScopeInfo scopeInfo;

    @Helper
    private ExampleHelper exampleHelper;

    public AnotherExampleHelper(ScopeInfo info) {
        this.scopeInfo = info;
    }

    public ExampleHelper getExampleHelper() {
        return exampleHelper;
    }
}
