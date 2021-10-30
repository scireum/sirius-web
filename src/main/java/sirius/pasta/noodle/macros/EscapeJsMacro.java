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
import sirius.pasta.noodle.sandbox.PublicAPI;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Escapes all line breaks and ' within the given string.
 * <p>
 * This can be used to ensure that the generated content can be placed in a string literal
 * in JavaScript.
 */
@Register
@PublicAPI
public class EscapeJsMacro extends BasicMacro {

    @Part
    private static Resources resources;

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
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
                    .replace("\r", "\\r")
                    .replace("\n", "\\n")
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

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }
}
