/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents a call to {@link  Sirius#isFrameworkEnabled(String)}.
 * <p>
 * Note that multiple frameworks can be requrested using {@code framework1,framework2}. The macro will return
 * <tt>true</tt> if one of the given frameworks is enabled.
 */
@Register
@PublicApi
public class IsFrameworkEnabledMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        String frameworks = (String) args[0];
        if (Strings.isEmpty(frameworks)) {
            return true;
        }
        for (String framework : frameworks.split(",")) {
            if (Sirius.isFrameworkEnabled(framework.trim())) {
                return true;
            }
        }

        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "isFrameworkEnabled";
    }

    @Override
    public String getDescription() {
        return "Returns true, if one of the given frameworks (as comma separated list) is enabled, false otherwise.";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
