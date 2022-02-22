/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;
import sirius.web.util.LinkBuilder;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Permits to create a {@link LinkBuilder} by using <tt>{@literal @}linkBuilder</tt> within a Tagliatelle template.
 */
@PublicApi
@Register
public class LinkBuilderMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return LinkBuilder.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return new LinkBuilder((String) args[0]);
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

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
