/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.compiler;

import parsii.tokenizer.Char;
import parsii.tokenizer.ParseError;
import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.CompileError;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.noodle.compiler.SourceCodeInfo;
import sirius.pasta.tagliatelle.Tagliatelle;
import sirius.pasta.tagliatelle.Template;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.Emitter;
import sirius.pasta.tagliatelle.emitter.InvokeTemplateEmitter;
import sirius.pasta.tagliatelle.tags.TagHandler;
import sirius.pasta.tagliatelle.tags.TagHandlerFactory;
import sirius.pasta.tagliatelle.tags.TaglibTagHandler;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * Provides a context for compiling a <tt>.pasta</tt> file.
 * <p>
 * The main responsibility is to collect all errors or warnings and to provide the stack context (the names of all
 * arguments and local variables).
 */
public class TemplateCompilationContext extends CompilationContext {

    private static final String PRAGMA_DEPRECATED = "deprecated";

    /**
     * Contains the context of the template which was being compiled and triggered the compilation of this template.
     * <p>
     * This is used to abort when a cyclic dependency is detected.
     */
    private final TemplateCompilationContext parent;

    /**
     * Contains the resulting template which is created and populated by the compiler.
     */
    private final Template template;

    @Part
    private static GlobalContext ctx;

    @Part
    private static Tagliatelle engine;

    /**
     * Creates a new context for the given template and parent context.
     *
     * @param template the template which is being compiled
     * @param parent   the parent context if the template is compiled because it is invoked by another template being
     *                 compiled
     */
    public TemplateCompilationContext(@Nonnull Template template,
                                      @Nonnull SourceCodeInfo sourceCodeInfo,
                                      @Nullable TemplateCompilationContext parent) {
        super(sourceCodeInfo);
        this.template = template;
        this.parent = parent;
    }

    /**
     * Returns the template being compiled.
     *
     * @return the template which is currently being compiled
     */
    public Template getTemplate() {
        return template;
    }

    /**
     * Resolves a tag name like <tt>i:if</tt> into a {@link TagHandler}.
     * <p>
     * There are mainly two types of tags. Built in ones, which start with <tt>i:</tt> and user defined tag libraries
     * which start with an arbitrary prefix.
     *
     * @param position the position where the tag was detected
     * @param tagName  the name of the tag to resolve
     * @return a tag handler when processes the referenced tag or <tt>null</tt> to indicate that this tag is unhandled.
     */
    public TagHandler findTagHandler(Char position, String tagName) {
        if (!tagName.contains(":")) {
            return null;
        }

        if (tagName.startsWith("i:")) {
            TagHandlerFactory factory = ctx.getPart(tagName, TagHandlerFactory.class);
            if (factory != null) {
                return factory.createHandler();
            } else {
                error(position, "Cannot find a handler for the internal tag: %s", tagName);
                return null;
            }
        }

        String prefix = Strings.split(tagName, ":").getFirst();
        if (!engine.isTaglib(prefix)) {
            return null;
        }

        try {
            Optional<TaglibTagHandler> tagHandler =
                    resolveTemplate(position, engine.resolveTagName(tagName)).map(TaglibTagHandler::new);
            if (tagHandler.isPresent()) {
                return tagHandler.get();
            } else {
                error(position, "Cannot find a template for the tag: %s", tagName);
                return null;
            }
        } catch (CompileException e) {
            error(position, "Error compiling referenced tag: %s%n%s", tagName, e.getMessage());
            return null;
        }
    }

    /**
     * Resolves the given name into a template.
     * <p>
     * If the template isn't compiled yet and directly or indirectly references the template currently being compiled,
     * an appropriate exception is thrown to avoid infinite recursion.
     *
     * @param position the position where the template was referenced
     * @param name     the name of the template to resolve
     * @return the resolved template or an empty optional to signal, that no matching template was found
     * @throws CompileException if case of a compilation error when compiling the referenced template
     */
    public Optional<Template> resolveTemplate(Position position, String name) throws CompileException {
        failOnRecursiveCompilation(position, name);
        return engine.resolve(name, this);
    }

    private void failOnRecursiveCompilation(Position position, String name) throws CompileException {
        TemplateCompilationContext compilationContext = this;
        StringBuilder loop = new StringBuilder();
        while (compilationContext != null) {
            if (loop.length() > 0) {
                loop.append("; ");
            }
            loop.append(compilationContext.getTemplate().getName());
            if (Strings.areEqual(name, compilationContext.template.getName())) {
                ParseError parseError =
                        ParseError.error(position, Strings.apply("Recursive template dependency: %s", loop));
                throw CompileException.create(compilationContext.getSourceCodeInfo().getName(),
                                              compilationContext.getSourceCodeInfo().getLocation(),
                                              Collections.singletonList(new CompileError(parseError, null)));
            }
            compilationContext = compilationContext.parent;
        }
    }

    /**
     * Generates an <tt>emitter</tt> which invokes the given template at runtime.
     * <p>
     * Note that only the template name, not the template itself is stored, so that modifications of the referenced
     * template will be detected and applied.
     *
     * @param position  the position where the invokation took place
     * @param template  the template to call
     * @param arguments the arguments passed to the template
     * @param blocks    the emitter blocks passed to the template
     * @return an appropriate emitter which invokes the template with the given argument expressions at runtime
     */
    public Emitter invokeTemplate(Position position,
                                  Template template,
                                  Function<String, Callable> arguments,
                                  Map<String, Emitter> blocks) {
        outputTemplateDeprecationWarning(position, template);
        InvokeTemplateEmitter emitter = new InvokeTemplateEmitter(position, template.getName());

        if (!template.getArguments().isEmpty()) {
            Callable[] args = collectArgumentsForInvoke(position, template, arguments);
            emitter.setArguments(args);
        }

        emitter.setBlocks(blocks);
        return emitter;
    }

    private Callable[] collectArgumentsForInvoke(Position position,
                                                 Template template,
                                                 Function<String, Callable> arguments) {
        Callable[] args = new Callable[template.getArguments().size()];
        int index = 0;
        for (TemplateArgument arg : template.getArguments()) {
            Callable argumentExpression = arguments.apply(arg.getName());
            if (argumentExpression != null) {
                if (!isAssignableTo(argumentExpression.getType(), arg.getType())) {
                    error(position,
                          "Incompatible attribute types. '%s' expects %s for '%s', but %s was given.",
                          template.getName(),
                          arg.getType(),
                          arg.getName(),
                          argumentExpression.getType());
                }
                outputArgumentDeprecationWarning(position, arg);
            }
            args[index] = argumentExpression;
            index++;
        }
        return args;
    }

    private void outputArgumentDeprecationWarning(Position position, TemplateArgument arg) {
        if (arg.getDeprecationWarning() != null) {
            warning(position, "The attribute '%s' is deprecated: %s", arg.getName(), arg.getDeprecationWarning());
        }
    }

    private void outputTemplateDeprecationWarning(Position position, Template template) {
        if (Strings.isFilled(template.getPragma(PRAGMA_DEPRECATED))) {
            warning(position,
                    "The template '%s' is deprecated: %s",
                    template.getShortName(),
                    template.getPragma(PRAGMA_DEPRECATED));
        }
    }
}
