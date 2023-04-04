/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.macros;

import sirius.kernel.Sirius;
import sirius.kernel.di.std.Register;
import sirius.kernel.info.Product;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.macros.BasicMacro;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Computes a base URI to an asset which is constant during runtime.
 * <p>
 * This will yield a string like <tt>/assets/dynamic/MD5</tt>. The hash is based on the current software version.
 * The responses to these URLs will be cached infinitely and thus optimize the server load and user experience.
 * <p>
 * Note however, that this must not be used for assets which may change at runtime as this would be hidden
 * by the caching. Also note, that in {@link Sirius#isDev() development environments} we always yield
 * a <tt>no-cache</tt> URI.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class StaticAssetUriMacro extends BasicMacro {

    @Override
    protected Class<?> getType() {
        return String.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("No Arguments are expected.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        if (Sirius.isDev()) {
            return "/assets/no-cache";
        } else {
            return "/assets/dynamic/" + Product.getProduct().getUniqueVersionString();
        }
    }

    @Override
    public String getDescription() {
        return "Determines the asset base URI for an asset which will not change at runtime.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "staticAssetUri";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }
}
