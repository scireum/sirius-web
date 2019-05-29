/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;
import sirius.web.util.LinkBuilder;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Permits to create a {@link LinkBuilder} by using <tt>{@literal @}linkBuilder</tt> within a Tagliatelle template.
 */
public class LinkBuilderMacro implements Macro {
    @Override
    public Class<?> getType() {
        return LinkBuilder.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        String value = (String) args[0].eval(ctx);
        return new LinkBuilder(value);
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Creates a link builder for the given parameter.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "linkBuilder";
    }
}
