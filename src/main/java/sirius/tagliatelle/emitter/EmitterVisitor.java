/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

/**
 * Created by aha on 19.05.17.
 */
public interface EmitterVisitor {
    Emitter visit(Emitter emitter);
}
