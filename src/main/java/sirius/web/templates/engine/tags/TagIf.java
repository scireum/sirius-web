/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.tags;

import sirius.web.templates.engine.emitter.CompositeEmitter;
import sirius.web.templates.engine.emitter.ConditionalEmitter;

/**
 * Created by aha on 12.05.17.
 */
public class TagIf extends TagHandler {
    @Override
    public void apply(TagHandler parentHandler, CompositeEmitter block) {
        ConditionalEmitter result = new ConditionalEmitter();
        result.setConditionExpression(getAttribute("test"));
        result.setWhenTrue(body);
        result.setWhenFalse(getBlock("else"));
        block.addChild(result);
    }
}
