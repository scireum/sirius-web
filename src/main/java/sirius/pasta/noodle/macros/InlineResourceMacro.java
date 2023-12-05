/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;
import sirius.pasta.noodle.sandbox.SandboxMode;
import sirius.pasta.tagliatelle.Tagliatelle;
import sirius.pasta.tagliatelle.Template;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Optional;

/**
 * Inlines a complete resource file into a JavaScript string.
 * <p>
 * Most probably the {@link EscapeJsMacro} will be used to make the included contents safe within
 * a string literal in JavaScript.
 * <p>
 * If only a block of JS is to be included (e.g. to aggregate many JS files into one), use either
 * {@link sirius.pasta.tagliatelle.compiler.InvokeHandler} or {@link sirius.pasta.tagliatelle.compiler.IncludeHandler}.
 *
 * @see sirius.pasta.tagliatelle.compiler.InvokeHandler
 * @see sirius.pasta.tagliatelle.compiler.IncludeHandler
 * @see EscapeJsMacro
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class InlineResourceMacro extends BasicMacro {

    private static final String PASTA_SUFFIX = ".pasta";

    @Part
    private Resources resources;

    @Part
    private Tagliatelle tagliatelle;

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.getFirst(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        super.verify(context, position, args);

        if (args.getFirst().isConstant()) {
            String resourceName = String.valueOf(args.getFirst().getConstantValue());
            if (resources.resolve(resourceName).isEmpty()) {
                context.warning(position, "Unknown resource: %s", resourceName);
            }
        }

        if (context.getSandboxMode() != SandboxMode.DISABLED) {
            if (!args.getFirst().isConstant()) {
                raiseSandboxRestriction(context,
                                        position,
                                        "Only constant resource paths are supported due to sandbox restrictions.");
            } else if (String.valueOf(args.getFirst().getConstantValue()).endsWith(PASTA_SUFFIX)) {
                raiseSandboxRestriction(context,
                                        position,
                                        "Only constant resources are supported due to sandbox restrictions.");
            }
        }
    }

    private void raiseSandboxRestriction(CompilationContext context, Position position, String message) {
        if (context.getSandboxMode() == SandboxMode.WARN_ONLY) {
            context.warning(position, message);
        } else {
            context.error(position, message);
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        String path = (String) args[0];
        if (!path.startsWith("/assets")) {
            throw new IllegalArgumentException("Only assets can be inlined for security reasons.");
        }

        if (path.endsWith(PASTA_SUFFIX)) {
            try {
                Template template = tagliatelle.resolve(path)
                                               .orElseThrow(() -> new IllegalArgumentException(
                                                       "Cannot find the referenced template: " + path));
                return template.renderToString();
            } catch (Exception e) {
                throw new IllegalArgumentException(Strings.apply(
                        "Failed to inline template '%s' as resource string: %s (%s)",
                        path,
                        e.getMessage(),
                        e.getClass().getName()));
            }
        }

        Optional<Resource> res = resources.resolve(path);
        return res.map(Resource::getContentAsString).orElse("");
    }

    @Nonnull
    @Override
    public String getName() {
        return "inlineResource";
    }

    @Override
    public String getDescription() {
        return "Returns the contents of the given asset as string. Use escapeJS() to place the data in a JS string.";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        if (!args.getFirst().isConstant()) {
            return false;
        }

        return !String.valueOf(args.getFirst().getConstantValue()).endsWith(PASTA_SUFFIX);
    }
}
