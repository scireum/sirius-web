/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Created by aha on 16.05.17.
 */
public abstract class GlobalRenderContext {
    protected Map<String, Template> templateCache;
    protected RenderStack stack = new RenderStack();
    protected List<Object> globals;
    protected Engine engine;

    protected GlobalRenderContext(Engine engine) {
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

    protected abstract boolean isAcceptingBytes();

    protected abstract void outputString(String string) throws IOException;

    protected abstract void outputBytes(byte[] bytes) throws RenderException, IOException;

    public LocalRenderContext createContext(Template template) {
        return new LocalRenderContext(template, this, stack.alloc(template.getNumberOfArguments()));
    }

    public void release(LocalRenderContext renderContext) {
        stack.free(renderContext.getLocals());
    }
}
