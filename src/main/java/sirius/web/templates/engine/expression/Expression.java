/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.web.templates.engine.RenderContext;

/**
 * Created by aha on 10.05.17.
 */
public abstract class Expression {

    public abstract Object eval(RenderContext ctx);

    public abstract Class<?> getType();

}
