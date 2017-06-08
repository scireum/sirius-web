/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.Sirius;
import sirius.kernel.cache.Cache;
import sirius.kernel.cache.CacheManager;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Log;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.compiler.CompileException;
import sirius.tagliatelle.compiler.Compiler;
import sirius.tagliatelle.rendering.GlobalRenderContext;
import sirius.web.templates.Resource;
import sirius.web.templates.Resources;
import sirius.web.templates.Templates;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Register(classes = Engine.class)
public class Engine {

    public static final Log LOG = Log.get("tagliatelle");

    private Map<String, Class<?>> aliases;
    private List<Tuple<String, Class<?>>> globalVariables;

    @Parts(RenderContextExtender.class)
    private Collection<RenderContextExtender> contextExtenders;

    @Parts(ClassAliasProvider.class)
    private Collection<ClassAliasProvider> aliasProviders;

    @Part
    private Resources resources;

    private Cache<Resource, Template> compiledTemplates = CacheManager.createCache("tagliatelle-templates");

    public Map<String, Class<?>> getClassAliases() {
        if (aliases == null) {
            Map<String, Class<?>> aliasMap = new HashMap<>();
            aliasProviders.forEach(p -> p.collectAliases(aliasMap::put));
            aliases = aliasMap;
        }
        return aliases;
    }

    public List<Object> getEnvironment() {
        List<Object> result = new ArrayList<>();
        contextExtenders.forEach(renderContextExtender -> renderContextExtender.collectParameterValues(result::add));
        if (Sirius.isDev()) {
            verifyGlobals(result);
        }

        return result;
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

    public CompilationContext createCompilationContext(String path, Resource resource, CompilationContext parent) {
        Template template = new Template(path, resource);
        return new CompilationContext(template, parent);
    }

    public GlobalRenderContext createRenderContext() {
        return new GlobalRenderContext(this);
    }

    private void verifyGlobals(List<Object> result) {
        int index = 0;
        for (Tuple<String, Class<?>> global : globalVariables) {
            if (result.size() <= index) {
                Templates.LOG.WARN("An invalid global environment was created! Missing a value for %s",
                                   global.getFirst());
            } else {
                Object value = result.get(index);
                if (!isAssignable(value, global.getSecond())) {
                    Templates.LOG.WARN("An invalid global environment was created!"
                                       + " An invalid value (%s) of type %s was created for parameter %s."
                                       + " But %s is the expected type.",
                                       value,
                                       value.getClass(),
                                       global.getFirst(),
                                       global.getSecond());
                }
            }
            index++;
        }
    }

    public static boolean isAssignable(Object from, Class<?> to) {
        if (from == null) {
            return !to.isPrimitive();
        }

        return isAssignableTo(from.getClass(), to);
    }

    public static boolean isAssignableTo(Class<?> from, Class<?> to) {
        if (to.isAssignableFrom(from)) {
            return true;
        }

        if (from.isPrimitive()) {
            return checkAutoboxing(from, to);
        } else {
            return checkAutoboxing(to, from);
        }
    }

    private static boolean checkAutoboxing(Class<?> primitveType, Class<?> boxedType) {
        if (primitveType == int.class || primitveType == long.class) {
            return Integer.class.isAssignableFrom(boxedType);
        }

        if (primitveType == boolean.class) {
            return Boolean.class.isAssignableFrom(boxedType);
        }

        return false;
    }

    public Optional<Template> resolve(String path) throws CompileException {
        return resolve(path, null);
    }

    public Optional<Template> resolve(String path, CompilationContext parentContext) throws CompileException {
        Optional<Resource> optionalResource = resources.resolve(path);
        if (!optionalResource.isPresent()) {
            return Optional.empty();
        }

        Resource resource = optionalResource.get();

        Template result = compiledTemplates.get(resource);
        if (result != null && result.getCompilationTimestamp() >= resource.getLastModified()) {
            return Optional.of(result);
        }

        CompilationContext compilationContext = createCompilationContext(path, resource, parentContext);

        new Compiler(compilationContext, resource.getContentAsString()).compile();
        compiledTemplates.put(resource, compilationContext.getTemplate());

        return Optional.of(compilationContext.getTemplate());
    }

    public String resolveTagName(String qualifiedTagName) {
        Tuple<String, String> tagName = Strings.split(qualifiedTagName, ":");
        return "/taglib/" + tagName.getFirst() + "/" + tagName.getSecond() + ".html.pasta";
    }
}
