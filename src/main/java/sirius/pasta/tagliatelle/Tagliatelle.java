/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle;

import parsii.tokenizer.ParseError;
import parsii.tokenizer.Position;
import sirius.kernel.Sirius;
import sirius.kernel.cache.Cache;
import sirius.kernel.cache.CacheEntry;
import sirius.kernel.cache.CacheManager;
import sirius.kernel.commons.MultiMap;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.pasta.Pasta;
import sirius.pasta.noodle.compiler.CompileError;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.noodle.compiler.SourceCodeInfo;
import sirius.pasta.tagliatelle.compiler.TemplateCompilationContext;
import sirius.pasta.tagliatelle.compiler.TemplateCompiler;
import sirius.pasta.tagliatelle.rendering.GlobalRenderContext;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.templates.Templates;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Provides statically compiled and optimized templates to generate HTML, XML and text files.
 * <p>
 * Helps to resolve and compile templates and also to generate a context used to render their output.
 */
@Register(classes = Tagliatelle.class)
public class Tagliatelle {

    protected static final String PRAGMA_ALIAS = "alias";

    @Part
    private Resources resources;

    @Part
    private Templates templates;

    /**
     * Keeps compiled templates around to improve the speed of rendering.
     */
    private final Cache<Resource, Template> compiledTemplates = CacheManager.createLocalCache("tagliatelle-templates");

    private MultiMap<String, String> taglibTags;

    private final Map<String, List<TemplateExtension>> extensions = new HashMap<>();

    /**
     * Returns all taglibs and all tags within this taglib.
     *
     * @return a multimap containing all taglibs (prefix) and their tags
     */
    public MultiMap<String, String> getTagLibTags() {
        if (taglibTags == null) {
            MultiMap<String, String> result = MultiMap.createOrdered();
            Sirius.getClasspath()
                  .find(Pattern.compile("(default/|customizations/[^/]+/)?taglib/([a-z]+)/([^.]*).*.pasta"))
                  .forEach(m -> result.put(m.group(2), m.group(3)));
            taglibTags = result;
        }

        return taglibTags;
    }

    /**
     * Returns all extensions available for the given target.
     *
     * @param target the target to lookup extensions for
     * @return a list of all extensions for the given target
     */
    public List<TemplateExtension> getExtensions(String target) {
        synchronized (extensions) {
            return Collections.unmodifiableList(extensions.computeIfAbsent(target, this::loadExtensions));
        }
    }

    private List<TemplateExtension> loadExtensions(String target) {
        return Sirius.getClasspath()
                     .find(Pattern.compile("(default/|customizations/[^/]+/)?extensions/"
                                           + Pattern.quote(target)
                                           + "/.*.pasta"))
                     .map(m -> m.group(0))
                     .map(this::resolveToTemplateExtension)
                     .filter(Objects::nonNull)
                     .sorted()
                     .collect(Collectors.toList());
    }

    private TemplateExtension resolveToTemplateExtension(String path) {
        try {
            Optional<Template> template = resolve("/" + path);
            if (!template.isPresent()) {
                throw Exceptions.handle()
                                .to(Pasta.LOG)
                                .withSystemErrorMessage("Cannot resolve extension '%s' into a template!", path)
                                .handle();
            }
            return new TemplateExtension(template.get());
        } catch (CompileException e) {
            throw Exceptions.handle()
                            .to(Pasta.LOG)
                            .error(e)
                            .withSystemErrorMessage("Failed to load extension %s: %s (%s)", path)
                            .handle();
        }
    }

    /**
     * Returns a list of all tag lib prefixes and descriptions.
     *
     * @return a list of tuples containing the taglib prefix and a short description
     */
    public List<Tuple<String, String>> getTagLibs() {
        return getTagLibTags().keySet()
                              .stream()
                              .map(name -> Tuple.create(name,
                                                        Sirius.getSettings()
                                                              .get("tagliatelle.taglib." + name)
                                                              .asString(name)))
                              .collect(Collectors.toList());
    }

    /**
     * Determines if a taglib with the given prefix exists.
     *
     * @param prefix the prefix to check
     * @return <tt>true</tt> if a taglib with the given prefix exists, <tt>false</tt>  otherwise
     */
    public boolean isTaglib(String prefix) {
        return getTagLibTags().getUnderlyingMap().containsKey(prefix);
    }

    /**
     * Creates a new {@link TemplateCompilationContext} for the given path and resource.
     *
     * @param path     the path of the template being compiled
     * @param resource the actual resource which was used to determine the source code of the template
     * @param parent   if the compilation was started while compiling another template, its context is given here. This
     *                 is mainly used to detect and abort cyclic dependencies at compile time.
     * @return a new compilation context for the given resource
     */
    public TemplateCompilationContext createResourceCompilationContext(@Nonnull String path,
                                                                       @Nonnull Resource resource,
                                                                       @Nullable TemplateCompilationContext parent) {
        Template template = new Template(path, resource);
        return new TemplateCompilationContext(template, SourceCodeInfo.forResource(resource), parent);
    }

    /**
     * Creates a new {@link TemplateCompilationContext} for the given inline code.
     *
     * @param name   the name of the source code / orign
     * @param code   the actual template code to compile
     * @param parent if the compilation was started while compiling another template, its context is given here. This
     *               is mainly used to detect and abort cyclic dependencies at compile time.
     * @return a new compilation context for the given resource
     */
    public TemplateCompilationContext createInlineCompilationContext(@Nonnull String name,
                                                                     @Nonnull String code,
                                                                     @Nullable TemplateCompilationContext parent) {
        Template template = new Template(name, null);
        return new TemplateCompilationContext(template, SourceCodeInfo.forInlineCode(code), parent);
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
    public Optional<Template> resolve(String path, @Nullable TemplateCompilationContext parentContext)
            throws CompileException {
        ensureProperTemplatePath(path);
        Optional<Resource> optionalResource = resources.resolve(path);
        if (!optionalResource.isPresent()) {
            return Optional.empty();
        }

        Resource resource = optionalResource.get();

        if (Pasta.LOG.isFINE()) {
            Pasta.LOG.FINE("Resolving template for '%s' ('%s)'...", path, resource.getUrl());
        }

        Template template = resolveFromCache(path, resource);
        if (template != null) {
            return Optional.of(template);
        }

        template = compileTemplate(path, resource, parentContext);
        compiledTemplates.put(resource, template);

        return Optional.of(template);
    }

    /**
     * For security reasons we only invoke templates that end with .pasta.
     * <p>
     * Otherwise one could customize a template and try to include a configuration file or class file and therefore
     * obtain private data.
     *
     * @param path the path to check
     */
    public static void ensureProperTemplatePath(String path) {
        if (!path.endsWith(".pasta")) {
            throw new IllegalArgumentException("Tagliatelle templates must end with '.pasta'. Invalid path: " + path);
        }

        if (!path.startsWith("/") && !path.startsWith("/original:")) {
            if (Sirius.isStartedAsTest() || Sirius.isDev()) {
                throw new IllegalArgumentException("Tagliatelle templates should start with a '/'. Invalid path: "
                                                   + path);
            } else {
                Pasta.LOG.WARN("Tagliatelle templates should start with a '/'. Invalid path: " + path);
            }
        }
    }

    private CompileException createGeneralCompileError(Template template, String message) {
        ParseError parseError = ParseError.error(Position.UNKNOWN, message);
        CompileError compileError = new CompileError(parseError, null);
        return CompileException.create(template.getName(),
                                       Optional.of(template.getResource())
                                               .map(Resource::getUrl)
                                               .map(Object::toString)
                                               .orElse(null),
                                       Collections.singletonList(compileError));
    }

    private Template resolveFromCache(String path, Resource resource) {
        Template result = compiledTemplates.get(resource);
        if (result != null) {
            if (resource.getLastModified() <= result.getCompilationTimestamp()) {
                if (Pasta.LOG.isFINE()) {
                    Pasta.LOG.FINE("Resolved '%s' for '%s' from cache...", result, resource.getUrl());
                }
                return result;
            }
            if (Pasta.LOG.isFINE()) {
                Pasta.LOG.FINE(
                        "Resolved '%s' for '%s' from cache but the resource is newer than the compiled template "
                        + "(%s > %s, Delta: %s)....Recompiling!",
                        result,
                        path,
                        resource.getLastModified(),
                        result.getCompilationTimestamp(),
                        resource.getLastModified() - result.getCompilationTimestamp());
            }
        } else if (Pasta.LOG.isFINE()) {
            Pasta.LOG.FINE("Cannot resolve '%s' for '%s' from cache...", result, resource.getUrl());
        }
        return null;
    }

    private Template compileTemplate(String path, Resource resource, @Nullable TemplateCompilationContext parentContext)
            throws CompileException {
        TemplateCompilationContext compilationContext = createResourceCompilationContext(path, resource, parentContext);
        TemplateCompiler compiler = new TemplateCompiler(compilationContext);
        compiler.compile();
        writeWarningsToLog(compilationContext);

        return handleAliasing(compilationContext.getTemplate(), compilationContext);
    }

    private void writeWarningsToLog(TemplateCompilationContext compilationContext) {
        if (compilationContext.getErrors().isEmpty()) {
            return;
        }

        StringBuilder message = new StringBuilder();
        message.append("Warnings when compiling ")
               .append(compilationContext.getTemplate().getShortName())
               .append(":\n");
        compilationContext.getErrors().forEach(error -> message.append(error).append("\n\n"));
        message.append("Template: ");
        message.append(compilationContext.getTemplate().getName());
        message.append("\n");

        if (compilationContext.getTemplate().getResource() != null) {
            message.append("URL: ");
            message.append(compilationContext.getTemplate().getResource().getUrl());
            message.append("\n");
        }

        Pasta.LOG.WARN(message);
    }

    private Template handleAliasing(Template template, TemplateCompilationContext compilationContext)
            throws CompileException {
        Value alias = template.getPragma(PRAGMA_ALIAS);
        if (alias.isFilled()) {
            String aliasPath = alias.asString();
            if (aliasPath.contains(":")) {
                aliasPath = resolveTagName(aliasPath);
            }
            return resolve(aliasPath, compilationContext).orElseThrow(() -> createGeneralCompileError(template,
                                                                                                      "Cannot resolve alias: "
                                                                                                      + alias.asString()));
        }

        return template;
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
