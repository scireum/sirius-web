/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;

/**
 * Provides a macro which encodes the given resource as Base64 string.
 * <p>
 * This can be used in CSS like <tt>background: url(base64Resource('/assets/img.png'));</tt> or in IMG tags via
 * <tt>&lt;img src="@base64Resource('/assets/img.png')" /&gt;</tt>.
 */
@Register
public class Base64ResourceMacro extends Base64Macro {

    @Part
    private Resources resources;

    @Override
    protected void verifyPath(CompilationContext context, Position position, String path) {
        if (resources.resolve(path).isEmpty()) {
            context.warning(position, "Unknown resource: %s", path);
        }
    }

    @Override
    protected byte[] getContent(String path) {
        if (!path.startsWith("/assets/")) {
            throw new IllegalArgumentException("Only assets can be inlined for security reasons.");
        }

        return resources.resolve(path)
                        .orElseThrow(() -> new IllegalArgumentException("Unknown resource: " + path))
                        .getContent();
    }

    @Nonnull
    @Override
    public String getName() {
        return "base64Resource";
    }

    @Override
    public String getDescription() {
        return "Creates a base64 representation of the given resource to be included in IMG tags or CSS files";
    }
}
