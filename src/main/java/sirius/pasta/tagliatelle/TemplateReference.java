/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle;

import sirius.kernel.Sirius;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.tagliatelle.rendering.RenderException;
import sirius.web.resources.Resources;

/**
 * References a <tt>Tagliatelle</tt> {@link Template} to be used within a class.
 * <p>
 * Assists, when a template is used for "string interpolation" or other tasks, where the template is known
 * at compile time and also static. On production systems, this will render the template once and then keep a
 * cached instance around to handle the render calls. On development systems, the template is checked for changes
 * and reloaded if needed.
 */
public class TemplateReference {

    private final String path;
    private Template cachedTemplate;

    @Part
    private static Tagliatelle tagliatelle;

    /**
     * Builds a reference to the template with the given path.
     * <p>
     * Note that <tt>TemplateReference</tt> is intended to be kept around as static constant as the template
     * is only actually resolved on the first render call.
     *
     * @param templatePath the path to the template
     */
    public TemplateReference(String templatePath) {
        this.path = templatePath;
    }

    /**
     * Yields the referenced template.
     *
     * @return the template being reference
     */
    public Template fetchTemplate() {
        if (cachedTemplate == null) {
            return compileTemplate();
        }

        return cachedTemplate;
    }

    /**
     * Directly renders the referenced template using the given arguments and returns the resulting string.
     *
     * @param args the arguments to pass to the template
     * @return the string produced by <tt>Tagliatelle</tt>
     */
    public String render(Object... args) {
        try {
            return fetchTemplate().renderToString(args);
        } catch (RenderException e) {
            throw Exceptions.handle(Resources.LOG, e);
        }
    }

    private Template compileTemplate() {
        try {
            Template result = tagliatelle.resolve(path)
                                         .orElseThrow(() -> new IllegalArgumentException("Unknown template: " + path));
            if (Sirius.isProd()) {
                this.cachedTemplate = result;
            }

            return result;
        } catch (CompileException e) {
            throw Exceptions.handle(Resources.LOG, e);
        }
    }
}
