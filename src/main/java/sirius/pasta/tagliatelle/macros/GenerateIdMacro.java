/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.macros.BasicMacro;
import sirius.pasta.noodle.sandbox.PublicAPI;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;
import sirius.web.util.IdGeneratorContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Generates a new ID which is unique within this {@link LocalRenderContext}.
 */
@Register
@PublicAPI
public class GenerateIdMacro extends BasicMacro {
    @Override
    protected Class<?> getType() {
        return String.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position pos, List<Class<?>> args) {
        if (args.size() > 1) {
            throw new IllegalArgumentException("At most one argument is expected.");
        }
        if (args.size() == 1 && !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expects a format string as parameter");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        long localId = CallContext.getCurrent().get(IdGeneratorContext.class).generateLocalId();
        if (args.length == 1) {
            return Strings.apply((String) args[0], localId);
        } else {
            return String.valueOf(localId);
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Override
    public String getDescription() {
        return "Generates an ID which is unique within the template being rendered. If a format string is given, the ID is applied as first parameter.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "generateId";
    }
}
