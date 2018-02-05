/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.rendering;

import java.util.function.Function;

/**
 * Represents a simple callback used by {@link GlobalRenderContext#emitToString(RenderCall, Function)}.
 * <p>
 * This is essentially {@link Runnable} with the addition of {@link RenderException} as exception.
 */
public interface RenderCall {

    /**
     * Invoked by the global context once all emitted content will be captured into a string.
     *
     * @throws RenderException in case of an exception while rendering
     */
    void render() throws RenderException;

}
