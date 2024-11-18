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
 * Provides a macro which encodes the given SVG resource as UTF-8 string, after optionally modifying the tint color.
 * <p>
 * The tint color, if set, is used to replace occurrences of black <tt>#000000</tt>.
 * <p>
 * The macro can be used in CSS like <tt>background: url(svgResource('/assets/img.svg', '#ff0000'));</tt> or in IMG tags
 * via <tt>&lt;img src="@svgResource('/assets/img.svg', '#ff0000')" /&gt;</tt>.
 *
 * @see InlineSvgResourceMacro
 */
@Register
public class SvgResourceMacro extends SvgMacro {

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
        if (!path.startsWith("/assets/")) {
            throw new IllegalArgumentException("Only assets can be inlined for security reasons.");
        }

        // parse XML, clean whitespace and XML declaration, and synthesize a string representation
        Resource resource =
                resources.resolve(path).orElseThrow(() -> new IllegalArgumentException("Unknown resource: " + path));

        return parseDocument(resource, "svg");
    }

    @Nonnull
    @Override
    public String getName() {
        return "svgResource";
    }

    @Override
    public String getDescription() {
        return "Creates a UTF-8 representation of the given SVG resource, possibly changed to the given tint color, to be included in IMG tags or CSS files";
    }
}
