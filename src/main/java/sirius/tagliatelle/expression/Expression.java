/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.rendering.LocalRenderContext;

/**
 * Created by aha on 10.05.17.
 */
public abstract class Expression {

    public abstract Object eval(LocalRenderContext ctx);

    public abstract Expression visit(ExpressionVisitor visitor);

    public abstract Expression reduce();

    public abstract boolean isConstant();

    public abstract Expression copy();

    public abstract Class<?> getType();
}
