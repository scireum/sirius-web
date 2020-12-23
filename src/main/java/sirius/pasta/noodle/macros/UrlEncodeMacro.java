/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.sandbox.PublicAPI;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>urlEncode(String)</tt> which is a call to {@link Strings#urlEncode(String)}.
 */
@Register
@PublicAPI
public class UrlEncodeMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("One parameter of type String is expected");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Strings.urlEncode((String) args[0]);
    }

    @Nonnull
    @Override
    public String getName() {
        return "urlEncode";
    }

    @Override
    public String getDescription() {
        return "Returns a url encoded representation of the given string";
    }
}
