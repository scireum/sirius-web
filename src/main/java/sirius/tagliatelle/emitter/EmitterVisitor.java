/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import javax.annotation.Nonnull;

/**
 * Provides a <b>visitor pattern</b> for {@link Emitter emitters}.
 * <p>
 * The visitor is invoked for each inner emitter and may either return the given emitter or replace it with a new one.
 */
public interface EmitterVisitor {

    /**
     * Invoked for each emitter.
     *
     * @param emitter the emitter being visited
     * @return the new emitter, or the given one if nothing is to be changed
     */
    @Nonnull
    Emitter visit(@Nonnull Emitter emitter);
}
