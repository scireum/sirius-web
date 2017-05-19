/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.tagliatelle.TagContext;
import sirius.tagliatelle.expression.Expression;

/**
 * Created by aha on 18.05.17.
 */
public class DummyTagHandler extends TagHandler {

    @Override
    public void apply(TagContext context) {

    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        return Expression.class;
    }

}
