/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.expression;

import sirius.web.templates.tagliatelle.LocalRenderContext;

/**
 * Created by aha on 10.05.17.
 */
public abstract class Expression {

    public abstract Object eval(LocalRenderContext ctx);

    public abstract Class<?> getType();

}
