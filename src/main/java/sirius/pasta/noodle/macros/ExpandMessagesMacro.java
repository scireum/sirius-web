/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.web.controller.MessageExpanders;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Runs all {@link sirius.web.controller.MessageExpander message expanders} on the given string.
 *
 * @see MessageExpanders
 */
@Register
public class ExpandMessagesMacro extends BasicMacro {

    @Part
    private MessageExpanders messageExpanders;

    @Override
    protected Class<?> getType() {
        return String.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return messageExpanders.expand((String) args[0]);
    }

    @Override
    public String getDescription() {
        return "Executes all MessageExpanders on the given String and returns the result.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "expandMessage";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

}
