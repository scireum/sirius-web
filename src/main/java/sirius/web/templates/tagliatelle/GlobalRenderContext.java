/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Created by aha on 16.05.17.
 */
class GlobalRenderContext {
    private Map<String, Template> templateCache;
    private RenderStack stack = new RenderStack();
    protected List<Object> globals;
    protected Engine engine;
    protected Consumer<String> output;

    protected GlobalRenderContext(Engine engine, Consumer<String> consumer) {
        this.output = consumer;
        this.engine = engine;
        this.globals = engine.getEnvironment();
    }

    public Template resolve(String templateName) throws CompileException {
        if (templateCache != null) {
            Template result = templateCache.get(templateName);
            if (result != null) {
                return result;
            }
        }

        Template result = engine.resolve(templateName);
        if (templateCache == null) {
            templateCache = new HashMap<>();
        }

        templateCache.put(templateName, result);

        return result;
    }

    public LocalRenderContext createContext(Template template) {
        return new LocalRenderContext(template, this, stack.alloc(template.getNumberOfArguments()));
    }

    public void release(LocalRenderContext renderContext) {
        stack.free(renderContext.getLocals());
    }
}
