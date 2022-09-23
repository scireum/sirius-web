/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.pasta.tagliatelle.rendering.GlobalRenderContext;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;
import sirius.pasta.tagliatelle.rendering.RenderException;

import javax.annotation.Nonnull;
import java.io.IOException;

/**
 * Represents a part of a {@link sirius.pasta.tagliatelle.Template} which generates text output when the template is
 * rendered.
 */
public abstract class Emitter {

    /**
     * Contains the position in the source file where this emitter was created.
     */
    protected Position startOfBlock;

    /**
     * Contains a new emitter with the given position.
     *
     * @param startOfBlock the start position where the emitter was created
     */
    protected Emitter(@Nonnull Position startOfBlock) {
        this.startOfBlock = startOfBlock;
    }

    /**
     * Procudes the text output into the given context.
     *
     * @param context the render context which provides access to the local and global environment and also the target
     *                for the generated text
     * @throws RenderException in case of any exception which occurs during rendering
     */
    public void emit(@Nonnull LocalRenderContext context) throws RenderException {
        context.updatePosition(startOfBlock);
        try {
            emitToContext(context);
        } catch (Exception ex) {
            throw RenderException.create(context, ex);
        }
    }

    /**
     * Actually produces the output while relying on the caller to properly handle any exception.
     *
     * @param context the render context which provides access to the local and global environment and also the target
     *                for the generated text
     * @throws Exception all thrown exceptions are caught and converted into a {@link RenderException}
     */
    protected abstract void emitToContext(@Nonnull LocalRenderContext context) throws Exception;

    /**
     * Returns the start position within the source file.
     *
     * @return the start of the definition which created this emitter
     */
    @Nonnull
    public Position getStartOfBlock() {
        return startOfBlock;
    }

    /**
     * Performs static optimizations at compile time.
     * <p>
     * Certain emitters can perform optimizations (evaluate constant expressions, combine constant outputs etc.) to
     * create more efficient templates. This is especially important when a template is inlined, as this most probably
     * provides a higher level of optimizations.
     *
     * @return a reduced version of this emitter of the emitter itself, if no further optimizations are possible
     */
    @Nonnull
    public abstract Emitter reduce();

    /**
     * Emits escaped comments to the rendering context with {@link GlobalRenderContext.DebugLevel#DEBUG} level.
     *
     * @param context   the render context which provides access to the local and global environment and also the target
     *                  for the generated text
     * @param message   text to output with {@link GlobalRenderContext.DebugLevel#DEBUG}
     * @param arguments the parameters for be used for replacement
     * @see String#format(String, Object...)
     */
    public void emitDebugMessage(@Nonnull LocalRenderContext context, String message, Object... arguments) {
        emitMessage(context, GlobalRenderContext.DebugLevel.DEBUG, message, arguments);
    }

    /**
     * Emits escaped comments to the rendering context with {@link GlobalRenderContext.DebugLevel#TRACE} level.
     *
     * @param context   the render context which provides access to the local and global environment and also the target
     *                  for the generated text
     * @param message   text to output with {@link GlobalRenderContext.DebugLevel#TRACE}
     * @param arguments the parameters for be used for replacement
     * @see String#format(String, Object...)
     */
    public void emitTraceMessage(@Nonnull LocalRenderContext context, String message, Object... arguments) {
        emitMessage(context, GlobalRenderContext.DebugLevel.TRACE, message, arguments);
    }

    private void emitMessage(@Nonnull LocalRenderContext context,
                             GlobalRenderContext.DebugLevel level,
                             String message,
                             Object... arguments) {
        if (context.getGlobalContext().canEmitDebug(level)) {
            try {
                context.outputDebug(Strings.apply("SIRIUS:%s - %s",
                                                  level.toString(),
                                                  Strings.apply(message, arguments)));
            } catch (IOException e) {
                Exceptions.ignore(e);
            }
        }
    }
}
