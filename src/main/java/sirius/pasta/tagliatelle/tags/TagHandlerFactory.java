/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.di.std.Named;
import sirius.pasta.tagliatelle.TemplateArgument;

import java.util.List;

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

    /**
     * Returns arguments expected by this tag handler.
     * <p>
     * Used for documentation purposes.
     *
     * @return a list of parameters expected by this tag.
     */
    List<TemplateArgument> reportArguments();

    /**
     * Returns a short description of what the tag does.
     *
     * @return a short description of the tag
     */
    String getDescription();
}
