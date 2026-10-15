/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.commons.Urls;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>urlEncode(String)</tt> which is a call to {@link Urls#encode(String)}.
 * <p>
 * Note that a space is encoded as <tt>+</tt> rather than <tt>%20</tt> and a literal <tt>+</tt> as <tt>%2B</tt>.
 * This follows the <tt>application/x-www-form-urlencoded</tt> format used for query strings, which is also what
 * <tt>urlDecode</tt> expects. Therefore, this macro is meant for query parameters and must not be used for paths,
 * where a <tt>+</tt> is a literal character.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class UrlEncodeMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.getFirst(), String.class)) {
            throw new IllegalArgumentException("One parameter of type String is expected");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Urls.encode((String) args[0]);
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

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }
}
