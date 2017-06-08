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
 * Can be {@link sirius.kernel.di.std.Register registered} to provide instances which handle tags with the given name.
 */
public interface TagHandlerFactory extends Named {

    /**
     * Creates a new tag handler instance to process a tag with the given name.
     *
     * @return a new tag handler to process a tag
     */
    TagHandler createHandler();
}
