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
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;

/**
 * Provides a macro for inlining an SVG resource into a DOM tree by dropping the XML declaration from the beginning and
 * returning the rest.
 */
@Register
public class InlineSvgResourceMacro extends InlineSvgMacro {

    @Part
    private Resources resources;

    @Override
    protected void verifyPath(CompilationContext context, Position position, String path) {
        if (resources.resolve(path).isEmpty()) {
            context.warning(position, "Unknown resource: %s", path);
        }
    }

    @Override
    protected Document getSvgDocument(String path) {
        if (!path.startsWith("/assets")) {
            throw new IllegalArgumentException("Only assets can be inlined for security reasons.");
        }

        Resource resource =
                resources.resolve(path).orElseThrow(() -> new IllegalArgumentException("Unknown resource: " + path));

        return parseDocument(resource, "svg");
    }

    @Nonnull
    @Override
    public String getName() {
        return "inlineSVG";
    }

    @Nonnull
    @Override
    public String getDescription() {
        return "Returns the root <svg> tag of an SVG resource as string, to be included in other DOM trees.";
    }
}
