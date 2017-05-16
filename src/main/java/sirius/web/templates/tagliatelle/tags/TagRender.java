/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.tags;

import sirius.web.templates.tagliatelle.TagContext;
import sirius.web.templates.tagliatelle.emitter.BlockEmitter;

/**
 * Created by aha on 12.05.17.
 */
public class TagRender extends TagHandler {

    @Override
    public void apply(TagContext context) {
        context.getBlock().addChild(new BlockEmitter(getConstantAttribute("name").asString()));
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
