/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import org.serversass.ast.FunctionCall;
import parsii.tokenizer.Position;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.web.dispatch.SassFunction;
import sirius.web.http.MimeHelper;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import java.util.Base64;
import java.util.List;

/**
 * Provides a macro which encodes the given resource as Base64 string.
 * <p>
 * This can be used in CSS like <tt>background: url(base64Resource('/assets/img.png'));</tt> or in IMG tags via
 * <tt>&lt;img src="@base64Resource('/assets/img.png')" /&gt;</tt>.
 */
@Register
public class Base64ResourceMacro extends BasicMacro implements SassFunction {

    @Part
    private Resources resources;

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
    public void verify(CompilationContext context, Position position, List<Node> args) {
        super.verify(context, position, args);

        if (args.get(0).isConstant()) {
            String resourceName = String.valueOf(args.get(0).getConstantValue());
            if (resources.resolve(resourceName).isEmpty()) {
                context.warning(position, "Unknown resource: %s", resourceName);
            }
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return args.get(0).isConstant();
    }

    @Override
    public String getDescription() {
        return "Creates a base64 representation of the given resource to be included in IMG tags or CSS files";
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        String path = (String) args[0];
        return encodeResource(path);
    }

    @Nonnull
    private String encodeResource(String path) {
        if (!path.startsWith("/assets")) {
            throw new IllegalArgumentException("Only assets can be inlined for security reasons.");
        }

        Resource resource = resources.resolve(path).orElse(null);
        if (resource == null) {
            return "";
        }

        String base64Data = Base64.getEncoder().encodeToString(resource.getContent());

        return "data:" + MimeHelper.guessMimeType(path) + ";base64," + base64Data;
    }

    @Override
    public String eval(FunctionCall call) {
        String path = call.getExpectedParam(0).toString();
        if (path.startsWith("'") && path.endsWith("'")) {
            path = path.substring(1, path.length() - 1);
        }

        return encodeResource(path);
    }

    @Nonnull
    @Override
    public String getName() {
        return "base64Resource";
    }
}
