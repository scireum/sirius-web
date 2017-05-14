/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.tags;

import sirius.web.templates.engine.emitter.CompositeEmitter;

/**
 * Created by aha on 12.05.17.
 */
public class TagBlock extends TagHandler {
    @Override
    public void apply(TagHandler parentHandler, CompositeEmitter block) {
        if (parentHandler != null) {
            parentHandler.addBlock(getConstantAttribute("name").asString(), body);
        }
    }
}
