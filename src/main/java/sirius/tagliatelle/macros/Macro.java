/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Named;
import sirius.tagliatelle.LocalRenderContext;
import sirius.tagliatelle.expression.Expression;

import java.util.List;

/**
 * Created by aha on 16.05.17.
 */
public interface Macro extends Named {

    Class<?> getType();

    void verifyArguments(List<Expression> args);

    Object eval(LocalRenderContext ctx, Expression[] args);

    boolean isConstant();
}
