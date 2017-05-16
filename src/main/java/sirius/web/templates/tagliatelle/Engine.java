/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle;

import sirius.kernel.Sirius;
import sirius.kernel.cache.Cache;
import sirius.kernel.cache.CacheManager;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.Register;
import sirius.web.security.ScopeInfo;
import sirius.web.security.UserContext;
import sirius.web.templates.Resource;
import sirius.web.templates.Resources;
import sirius.web.templates.Templates;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

/**
 * Created by aha on 15.05.17.
 */
@Register(classes = Engine.class)
public class Engine {

    private List<Tuple<String, Class<?>>> globalVariables;
    private Map<String, List<Object>> environments = new ConcurrentHashMap<>();

    @Parts(RenderContextExtender.class)
    private Collection<RenderContextExtender> contextExtenders;

    @Part
    private Resources resources;

    private Cache<Resource, Template> compiledTemplates = CacheManager.createCache("tagliatelle-templates");

    protected List<Object> getEnvironment() {
        ScopeInfo currentScope = UserContext.getCurrentScope();
        return Collections.unmodifiableList(environments.computeIfAbsent(currentScope.getScopeId(),
                                                                         id -> buildEnvironment()));
    }

    public List<Tuple<String, Class<?>>> getGlobalVariables() {
        if (globalVariables == null) {
            List<Tuple<String, Class<?>>> result = new ArrayList<>();
            contextExtenders.forEach(renderContextExtender -> renderContextExtender.collectParameterTypes((n, t) -> result
                    .add(Tuple.create(n, t))));
            globalVariables = Collections.unmodifiableList(result);
        }

        return globalVariables;
    }

    public CompilationContext createCompilationContext() {
        return new CompilationContext(this);
    }

    public GlobalRenderContext createRenderContext(Consumer<String> consumer) {
        return new GlobalRenderContext(this, consumer);
    }

    private List<Object> buildEnvironment() {
        List<Object> result = new ArrayList<>();
        contextExtenders.forEach(renderContextExtender -> renderContextExtender.collectParameterValues(result::add));
        if (Sirius.isDev()) {
            verifyGlobals(result);
        }
        return result;
    }

    private void verifyGlobals(List<Object> result) {
        int index = 0;
        for (Tuple<String, Class<?>> global : globalVariables) {
            if (result.size() <= index) {
                Templates.LOG.WARN("An invalid global environment was created! Missing a value for %s",
                                   global.getFirst());
            } else {
                Object value = result.get(index);
                if (value != null && !global.getSecond().isAssignableFrom(value.getClass())) {
                    Templates.LOG.WARN("An invalid global environment was created!"
                                       + " An invalid value (%s) of type %s was created for parameter %s."
                                       + " But %s is the expected type.",
                                       value,
                                       value.getClass(),
                                       global.getFirst(),
                                       global.getSecond());
                }
            }
        }
    }

    public Template resolve(String path) throws CompileException {
        Resource resource = resources.resolve(path).orElseThrow(() -> CompileException.create(null));
        Template result = compiledTemplates.get(resource);
        if (result != null && result.getCompilationTimestamp() >= resource.getLastModified()) {
            return result;
        }

        result = new Compiler(path, resource, resource.getContentAsString(), createCompilationContext()).compile();
        compiledTemplates.put(resource, result);

        return result;
    }

    public Template resolveTag(String qualifiedTagName) throws CompileException {
        Tuple<String, String> tagName = Strings.split(qualifiedTagName, ":");
        return resolve("/taglib/" + tagName.getFirst() + "/" + tagName.getSecond() + ".html.pasta");
    }
}
