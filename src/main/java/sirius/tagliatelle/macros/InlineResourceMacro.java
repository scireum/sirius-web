/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;
import sirius.web.templates.Resource;
import sirius.web.templates.Resources;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Optional;

/**
 * Inlines a complete resource file into a JavaScript string.
 * <p>
 * This means that the macro will return the whole contents of the file and escape all line breakes and ' within.
 */
@Register
public class InlineResourceMacro implements Macro {

    @Part
    private static Resources resources;

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        Optional<Resource> res = resources.resolve((String) args[0].eval(ctx));
        return res.map(r -> r.getContentAsString().replaceAll("\\r?\\n", " ").replace("'", "\\'")).orElse("");
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "inlineJavaScriptResource";
    }
}
