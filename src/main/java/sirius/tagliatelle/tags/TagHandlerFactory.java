/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Named;

/**
 * Created by aha on 18.05.17.
 */
public interface TagHandlerFactory extends Named {

    TagHandler createHandler();

}
