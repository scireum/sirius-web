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
import sirius.kernel.cache.CacheEntry;
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

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Provides statically compiled and optimized templates to generate HTML, XML and text files.
 * <p>
 * Helps to resolve and compile templates and also to generate a context used to render their output.
 */
@Register(classes = Tagliatelle.class)
public class Tagliatelle {

    /**
     * Logs everything related to resolving, compiling and rendering tagliatelle templates.
     */
    public static final Log LOG = Log.get("tagliatelle");

    /**
     * Contains all aliases collected via {@link ClassAliasProvider alias providers}.
     */
    private Map<String, Class<?>> aliases;

    /**
     * Contains all types of global variables collected via {@link RenderContextExtender context extenders}.
     */
    private List<Tuple<String, Class<?>>> globalVariables;

    @Parts(RenderContextExtender.class)
    private Collection<RenderContextExtender> contextExtenders;

    @Parts(ClassAliasProvider.class)
    private Collection<ClassAliasProvider> aliasProviders;

    @Part
    private Resources resources;

    /**
     * Keeps compiled templates around to improve the speed of rendering.
     */
    private Cache<Resource, Template> compiledTemplates = CacheManager.createCache("tagliatelle-templates");

    /**
     * Provides all known class aliases.
     *
     * @return an unmodifyable map of all known aliases for Java classes.
     */
    public Map<String, Class<?>> getClassAliases() {
        if (aliases == null) {
            Map<String, Class<?>> aliasMap = new HashMap<>();
            aliasProviders.forEach(p -> p.collectAliases(aliasMap::put));
            aliases = aliasMap;
        }
        return Collections.unmodifiableMap(aliases);
    }

    /**
     * Creates a new list of global variables used to initialize a global render context.
     *
     * @return a new list of global variables
     */
    public List<Object> createEnvironment() {
        List<Object> result = new ArrayList<>();
        contextExtenders.forEach(renderContextExtender -> renderContextExtender.collectParameterValues(result::add));
        if (Sirius.isDev()) {
            verifyGlobals(result);
        }

        return result;
    }

    /**
     * Ensures, that all {@link RenderContextExtender} produce a sane and expected output.
     *
     * @param environment the environment to check
     */
    private void verifyGlobals(List<Object> environment) {
        int index = 0;
        for (Tuple<String, Class<?>> global : globalVariables) {
            if (environment.size() <= index) {
                Templates.LOG.WARN("An invalid global environment was created! Missing a value for %s",
                                   global.getFirst());
            } else {
                Object value = environment.get(index);
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

    /**
     * Returns the names and types of the known global variables to assist the {@link Compiler}.
     *
     * @return the list of known global variables and their types
     */
    public List<Tuple<String, Class<?>>> getGlobalVariables() {
        if (globalVariables == null) {
            List<Tuple<String, Class<?>>> result = new ArrayList<>();
            contextExtenders.forEach(renderContextExtender -> renderContextExtender.collectParameterTypes((n, t) -> result
                    .add(Tuple.create(n, t))));
            globalVariables = Collections.unmodifiableList(result);
        }

        return globalVariables;
    }

    /**
     * Creates a new {@link CompilationContext} for the given path and resource.
     *
     * @param path     the path of the template being compiled
     * @param resource the actual resource which was used to determine the source code of the template
     * @param parent   if the compilation was started while compiling another template, its context is given here. This
     *                 is mainly used to detect and abort cyclic dependencies at compile time.
     * @return a new compilation context for the given resource
     */
    public CompilationContext createCompilationContext(@Nonnull String path,
                                                       @Nullable Resource resource,
                                                       @Nullable CompilationContext parent) {
        Template template = new Template(path, resource);
        return new CompilationContext(template, parent);
    }

    /**
     * Creates a new render context.
     *
     * @return a context used to render a template
     */
    public GlobalRenderContext createRenderContext() {
        return new GlobalRenderContext(this);
    }

    /**
     * Determines if the type <tt>to</tt> is assignable from the given object.
     * <p>
     * In contrast to {@link Class#isAssignableFrom(Class)}, this also handles autoboxing appropriately.
     *
     * @param from the object to assign
     * @param to   the type to assign to
     * @return <tt>true</tt> if the object is assignable, <tt>false</tt> otherwise
     */
    public static boolean isAssignable(Object from, Class<?> to) {
        if (from == null) {
            return !to.isPrimitive();
        }

        return isAssignableTo(from.getClass(), to);
    }

    /**
     * Determines if the type <tt>to</tt> is assignable from the given type <tt>from</tt>.
     * <p>
     * In contrast to {@link Class#isAssignableFrom(Class)}, this also handles autoboxing appropriately.
     *
     * @param from the type to assign
     * @param to   the type to assign to
     * @return <tt>true</tt> if the object is assignable, <tt>false</tt> otherwise
     */
    public static boolean isAssignableTo(Class<?> from, Class<?> to) {
        if (to.isAssignableFrom(from)) {
            return true;
        }

        // Null if represented as void.class and can be assigned to any non-primitive type.
        if (from == void.class) {
            return !to.isPrimitive();
        }

        if (from.isPrimitive()) {
            if (to.isPrimitive()) {
                return checkTypeConversion(from, to);
            }
            return checkAutoboxing(from, to);
        } else {
            return checkAutoboxing(to, from);
        }
    }

    private static boolean checkTypeConversion(Class<?> from, Class<?> to) {
        if (from == long.class && to == int.class || from == int.class && to == long.class) {
            return true;
        }

        return false;
    }

    /**
     * Determines if the boxed type matches the primitive type.
     *
     * @param primitveType the primitive type to check, e.g. <tt>int.class</tt>
     * @param boxedType    the boxed type to check, e.g. <tt>Integer.class</tt>
     * @return <tt>true</tt> if autoboxing would be successful for the given classes, <tt>false</tt> otherwise
     */
    private static boolean checkAutoboxing(Class<?> primitveType, Class<?> boxedType) {
        if (primitveType == int.class || primitveType == long.class) {
            return boxedType.isAssignableFrom(Integer.class);
        }

        if (primitveType == boolean.class) {
            return boxedType.isAssignableFrom(Boolean.class);
        }

        return false;
    }

    /**
     * Resolves the given path using {@link Resources} and compiles it into a {@link Template}.
     *
     * @param path the path to resolve
     * @return the appropriate template or an empty template if no matching {@link Resource} was found.
     * @throws CompileException in case of one or more compilation errors in the template
     */
    public Optional<Template> resolve(String path) throws CompileException {
        return resolve(path, null);
    }

    /**
     * Resolves the given path using {@link Resources} and compiles it into a {@link Template}.
     * <p>
     * If the template is resolved as part of another compilation process, the context is passed in as
     * <tt>parentContext</tt> to detect and abort cyclic references.
     *
     * @param path          the path to resolve
     * @param parentContext the outer compilation context which is in charge of compiling the callee
     * @return the appropriate template or an empty template if no matching {@link Resource} was found.
     * @throws CompileException in case of one or more compilation errors in the template
     */
    public Optional<Template> resolve(String path, @Nullable CompilationContext parentContext) throws CompileException {
        Optional<Resource> optionalResource = resources.resolve(path);
        if (!optionalResource.isPresent()) {
            return Optional.empty();
        }

        Resource resource = optionalResource.get();

        if (Tagliatelle.LOG.isFINE()) {
            Tagliatelle.LOG.FINE("Resolving template for '%s' ('%s)'...", path, resource.getUrl());
        }

        Template result = compiledTemplates.get(resource);
        if (result != null) {
            if (resource.getLastModified() <= result.getCompilationTimestamp()) {
                if (Tagliatelle.LOG.isFINE()) {
                    Tagliatelle.LOG.FINE("Resolved '%s' for '%s' from cache...", result, resource.getUrl());
                }
                return Optional.of(result);
            }
            if (Tagliatelle.LOG.isFINE()) {
                Tagliatelle.LOG.FINE(
                        "Resolved '%s' for '%s' from cache but the resource is newer than the compiled template (%s > %s, Delta: %s)....Recompiling!",
                        result,
                        path,
                        resource.getLastModified(),
                        result.getCompilationTimestamp(),
                        resource.getLastModified() - result.getCompilationTimestamp());
            }
        } else if (Tagliatelle.LOG.isFINE()) {
            Tagliatelle.LOG.FINE("Cannot resolve '%s' for '%s' from cache...", result, resource.getUrl());
        }

        CompilationContext compilationContext = createCompilationContext(path, resource, parentContext);

        new Compiler(compilationContext, resource.getContentAsString()).compile();
        compiledTemplates.put(resource, compilationContext.getTemplate());

        return Optional.of(compilationContext.getTemplate());
    }

    /**
     * Computes the effective path name for a tag.
     * <p>
     * A template for a tag named <tt>&lt;prefix:tagName&gt;</tt> is expected to reside in
     * <tt>/taglib/prefix/tagName.html.pasta</tt>.
     *
     * @param qualifiedTagName the qualified tag name, e.g. <tt>prefix:tagName</tt>
     * @return the template path, e.g. <tt>/taglib/prefix/tagName.html.pasta</tt>
     */
    public String resolveTagName(String qualifiedTagName) {
        Tuple<String, String> tagName = Strings.split(qualifiedTagName, ":");
        return "/taglib/" + tagName.getFirst() + "/" + tagName.getSecond() + ".html.pasta";
    }

    /**
     * Provides a list of all currently compiled templates.
     * <p>
     * Note that this directly accesses an inner cache. Therefore some templates which were rendered some time ago,
     * might have been dropped out of the cache and will therefore not occur in this list.
     *
     * @return a list of all compiled templates
     */
    public List<Template> getCompiledTemplates() {
        return compiledTemplates.getContents().stream().map(CacheEntry::getValue).collect(Collectors.toList());
    }
}
