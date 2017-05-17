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
import sirius.tagliatelle.LocalRenderContext;
import sirius.tagliatelle.RenderException;

/**
 * Created by aha on 10.05.17.
 */
public abstract class Emitter {

    protected Position startOfBlock;

    public Emitter(Position startOfBlock) {
        this.startOfBlock = startOfBlock;
    }

    public void emit(LocalRenderContext context) throws RenderException {
        context.updatePosition(startOfBlock);
        try {
            emitToContext(context);
        } catch (RenderException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new RenderException(Strings.apply("%s (%s)%n%nRender Stack%n-----------%n%s%n",
                                                    ex.getMessage(),
                                                    ex.getClass().getName(),
                                                    context), ex);
        }
    }

    protected abstract void emitToContext(LocalRenderContext context) throws Exception;
}
