/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.GlobalRenderContext;
import sirius.tagliatelle.rendering.LocalRenderContext;
import sirius.tagliatelle.rendering.RenderException;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.util.function.Function;

/**
 * Represents a part of a {@link sirius.tagliatelle.Template} which generates text output when the template is rendered.
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
     * Creates a deep copy of this emitter.
     * <p>
     * When templates are inlined, the parameters are propagated and constant expressions and conditions are evaluated
     * at compile time. This yields faster and more efficient templates. In order to perform these optimizations, we
     * need a copy of the template and all its emitters which can safely be modified.
     *
     * @return a deep copy of this emitter, which can safely be modified without changing this emitter
     */
    @Nonnull
    public abstract Emitter copy();

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
     * Visits all expressions contained in this emitter or its children.
     *
     * @param visitor a function which generates an {@link ExpressionVisitor} while supplying the current position (for
     *                improved error messages).
     */
    public abstract void visitExpressions(@Nonnull Function<Position, ExpressionVisitor> visitor);

    /**
     * Emits escaped comments to the rendering context
     *
     * @param context the render context which provides access to the local and global environment and also the target
     *                for the generated text
     * @param message text to output
     */
    public void emitDebugMessage(@Nonnull LocalRenderContext context, String message) {
        if (context.getGlobalContext().canEmitDebug(GlobalRenderContext.DebugLevel.DEBUG)) {
            try {
                context.outputDebug(Strings.apply("SIRIUS:%s - %s", context.getGlobalContext().getSiriusDebugLevel(), message));
            } catch (IOException e) {
                Exceptions.ignore(e);
            }
        }
    }
}
