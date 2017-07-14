/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;

import javax.annotation.Nonnull;

/**
 * Abstract superclass for all emitters which modify the stack (define local variables).
 */
public abstract class PushEmitter extends Emitter {

    protected int localIndex;

    /**
     * Contains a new emitter with the given position.
     *
     * @param startOfBlock the start position where the emitter was created
     */
    protected PushEmitter(@Nonnull Position startOfBlock) {
        super(startOfBlock);
    }

    /**
     * Contains the stack index being written to.
     *
     * @return the target index to write to
     */
    public int getLocalIndex() {
        return localIndex;
    }

    /**
     * Updates the stack index being written to.
     * <p>
     * When inlining a template, the stack has to be transferred to the callee and therefore the
     * stack indices might change.
     *
     * @param localIndex the new stack index to use
     */
    public void setLocalIndex(int localIndex) {
        this.localIndex = localIndex;
    }
}
