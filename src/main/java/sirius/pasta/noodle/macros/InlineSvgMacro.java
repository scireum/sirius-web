/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import org.w3c.dom.Document;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Provides a macro for inlining an SVG file into a DOM tree by dropping the XML declaration from the beginning and
 * returning the rest.
 */
@Register
public class InlineSvgMacro extends XmlProcessingMacro {

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
    public Object invoke(Environment environment, Object[] args) {
        String path = (String) args[0];
        return processResource(path);
    }

    @Nonnull
    private String processResource(String path) {
        if (!path.startsWith("/assets")) {
            throw new IllegalArgumentException("Only assets can be inlined for security reasons.");
        }

        Document document = parseSvgDocument(resources.resolve(path)
                                                      .orElseThrow(() -> new IllegalArgumentException(
                                                              "Unknown resource: " + path)));
        return stringifyElement(document.getDocumentElement(), false);
    }

    @Nonnull
    @Override
    public String getName() {
        return "inlineSVG";
    }

    @Nonnull
    @Override
    public String getDescription() {
        return "Returns the root <svg> tag of an SVG file as string, to be included in other DOM trees.";
    }
}
