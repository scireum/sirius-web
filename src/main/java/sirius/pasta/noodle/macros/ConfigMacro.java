/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Reads the system configuration using the given key.
 * <p>
 * This directly permits to access the system config (and is thus not part of the public API). However, note
 * that due to the static nature of the system, this call is considered constant and optimized away at compile
 * time.
 */
@Register
public class ConfigMacro extends BasicMacro {

    @Override
    protected Class<?> getType() {
        return Value.class;
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expects a single string as argument.");
        }

        if (args.get(0).isConstant()) {
            String configKey = (String) args.get(0).getConstantValue();
            if (!Sirius.getSettings().has(configKey)) {
                context.warning(position, "Unknown config value: '%s'", configKey);
            }
        }
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        throw new UnsupportedOperationException("unreachable");
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Sirius.getSettings().get((String) args[0]);
    }

    @Override
    public String getDescription() {
        return "Reads the given config setting.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "config";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
