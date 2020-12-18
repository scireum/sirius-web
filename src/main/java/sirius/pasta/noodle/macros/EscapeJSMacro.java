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
import sirius.pasta.noodle.sandbox.PublicAPI;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Escapes all line breakes and ' within the given string.
 */
@Register
@PublicAPI
public class EscapeJSMacro extends BasicMacro {

    @Part
    private static Resources resources;

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Class<?>> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("Expected a single argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        Object value = args[0];
        if (value == null) {
            return "";
        }

        return value.toString()
                    .replace("\\", "\\\\")
                    .replaceAll("\\r", "\\\\r")
                    .replaceAll("\\n", "\\\\n")
                    .replace("'", "\\'")
                    .replace("/", "\\/");
    }

    @Nonnull
    @Override
    public String getName() {
        return "escapeJS";
    }

    @Override
    public String getDescription() {
        return "Converts the given argument to an escaped JavaScript string by replacing linebreaks by blanks and ' by \\'";
    }
}
