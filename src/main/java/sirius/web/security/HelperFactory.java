/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

/**
 * Created by aha on 10.02.16.
 */
public interface HelperFactory<H> {
    Class<H> getHelperType();

    H make(ScopeInfo scope);
}
