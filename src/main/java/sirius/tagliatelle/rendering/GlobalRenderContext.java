/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.rendering;

import sirius.tagliatelle.Engine;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.compiler.CompileException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Created by aha on 16.05.17.
 */
public class GlobalRenderContext {
    protected Map<String, Template> templateCache;
    protected RenderStack stack = new RenderStack();
    protected List<Object> globals;
    protected Engine engine;
    protected StringBuilder buffer = new StringBuilder();

    public GlobalRenderContext(Engine engine) {
        this.engine = engine;
        this.globals = engine.getEnvironment();
    }

    public Optional<Template> resolve(String templateName) throws CompileException {
        if (templateCache != null) {
            Template result = templateCache.get(templateName);
            if (result != null) {
                return Optional.of(result);
            }
        }

        Optional<Template> result = engine.resolve(templateName);
        if (!result.isPresent()) {
            return result;
        }

        if (templateCache == null) {
            templateCache = new HashMap<>();
        }

        templateCache.put(templateName, result.get());

        return result;
    }

    protected void outputString(String string) {
        buffer.append(string);
    }

    public LocalRenderContext createContext(Template template) {
        return new LocalRenderContext(template, this, stack.alloc(template.getStackDepth()));
    }

    public void release(LocalRenderContext renderContext) {
        stack.free(renderContext.getLocals());
    }

    @Override
    public String toString() {
        return buffer.toString();
    }
}
