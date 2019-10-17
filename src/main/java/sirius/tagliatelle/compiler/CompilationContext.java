/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import parsii.tokenizer.Char;
import parsii.tokenizer.ParseError;
import parsii.tokenizer.Position;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.InvokeTemplateEmitter;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.tags.TagHandler;
import sirius.tagliatelle.tags.TagHandlerFactory;
import sirius.tagliatelle.tags.TaglibTagHandler;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * Provides a context for compiling a <tt>.pasta</tt> file.
 * <p>
 * The main responsibility is to collect all errors or warnings and to provide the stack context (the names of all
 * arguments and local variables).
 */
public class CompilationContext {

    private static final String PRAGMA_DEPRECATED = "deprecated";
    private static final int MAX_ERRORS = 50;

    private static class StackLocation {
        int stackIndex;
        Class<?> type;
        String name;
    }

    /**
     * Contains the context of the template which was being compiled and triggered the compilation of this template.
     * <p>
     * This is used to abort when a cyclic dependency is detected.
     */
    private final CompilationContext parent;

    /**
     * Contains the resulting template which is created and populated by the compiler.
     */
    private final Template template;

    /**
     * Contains the maximal number of arguments and local variables (stack depth) present at any single time.
     * <p>
     * This is used by the runtime to control memory allocation when a template is being rendered.
     */
    private int stackDepth = 0;

    /**
     * Contains a mapping of the name to the type for all known arguments and local variables.
     */
    private List<StackLocation> stack = new ArrayList<>();

    /**
     * Contains a mapping of the name to he type for all globals in the environment.
     */
    private List<Tuple<String, Class<?>>> globals;

    /**
     * Contains a list of errors and warnings which occured during compilation.
     */
    private List<ParseError> errors = new ArrayList<>();

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
    public CompilationContext(@Nonnull Template template, @Nullable CompilationContext parent) {
        this.globals = engine.getGlobalVariables();
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
     * Allocates a stack position for an argument or local variable.
     * <p>
     * For known and resolveable variables, this is automatically handled by {@link #push(Position, String, Class)}.
     * <p>
     * However, when a template is inlined into another, we also have to transfer locals to a new stack location and
     * might also create new intermediate locals for now inlined arguments (because we must not evaluate non-constant
     * expressions more thant once).
     *
     * @return the stack location to use
     */
    public int stackAlloc() {
        return stackDepth++;
    }

    /**
     * Pushes a local variable on the stack, so that it can be resolved later.
     *
     * @param position the position where the local is defined - mainly used for error reporting
     * @param name     the name of the variable
     * @param type     the type of the variable
     * @return the stack index of the variable
     */
    public int push(Position position, String name, Class<?> type) {
        if (Strings.isFilled(name)) {
            if (globals.stream().map(Tuple::getFirst).anyMatch(otherName -> Strings.areEqual(name, otherName))) {
                warning(position, "Argument or local variable hides a global: %s", name);
            }
            if (stack.stream()
                     .map(stackLocation -> stackLocation.name)
                     .anyMatch(otherName -> Strings.areEqual(name, otherName))) {
                warning(position, "Argument or local variable hides another one: %s", name);
            }
        }

        StackLocation location = new StackLocation();
        location.name = name;
        location.type = type;
        location.stackIndex = stack.size();

        stack.add(location);
        stackDepth = Math.max(stackDepth, stack.size());

        return location.stackIndex;
    }

    /**
     * Pops locals off the stack as long as their <tt>stackIndex</tt> is greater or equal than the one provided.
     * <p>
     * Note that this will only reduce the visibility of the variables but not free up the technical stack location. We
     * only used each stack location once, to greatly simplify inlining.
     *
     * @param position   the position which caused the popUntil - mainly used for error reporting
     * @param localIndex the limit to pop off to
     */
    public void popUntil(Position position, int localIndex) {
        while (stack.size() > localIndex) {
            stack.remove(stack.size() - 1);
        }
        if (stack.size() < localIndex) {
            error(position, "Cannot pop from empty stack");
        }
    }

    /**
     * Returns the maximum stack depth required when rendering this template.
     *
     * @return the maximal required stack depth to render this template
     */
    public int getStackDepth() {
        return stackDepth;
    }

    /**
     * Returns the "real" and visible stack depth.
     * <p>
     * In contrast to {@link #getStackDepth()} this grows <b>and shrinks</b> if a variable goes out of scope.
     *
     * @return the current number of visible arguments and local variables.
     */
    public int getVisibleStackDepth() {
        return stack.size();
    }

    /**
     * Tries to resolve a variable name into a stack location.
     *
     * @param variableName the name of the variable
     * @return the type and the stack location of the variable or an empty optional, if no such variable exists
     */
    public Optional<Tuple<Class<?>, Integer>> findLocal(String variableName) {
        for (int i = stack.size() - 1; i >= 0; i--) {
            StackLocation localVariable = stack.get(i);
            if (Strings.areEqual(variableName, localVariable.name)) {
                return Optional.of(Tuple.create(localVariable.type, localVariable.stackIndex));
            }
        }

        return Optional.empty();
    }

    /**
     * Tries to resolve a variable name into an environment index.
     *
     * @param variableName the variable to resolve
     * @return the type and the index in the environment list or an empty optional, if no such variable exists
     */
    public Optional<Tuple<Class<?>, Integer>> findGlobal(String variableName) {
        for (int i = globals.size() - 1; i >= 0; i--) {
            Tuple<String, Class<?>> globalVariable = globals.get(i);
            if (Strings.areEqual(variableName, globalVariable.getFirst())) {
                return Optional.of(Tuple.create(globalVariable.getSecond(), i));
            }
        }

        return Optional.empty();
    }

    /**
     * Records an error for the given position.
     *
     * @param pos     the position where the error occured
     * @param message the message to show
     * @param params  the formatting parameters applied to the message
     */
    public void error(Position pos, String message, Object... params) {
        if (!Sirius.isStartedAsTest()
            && !errors.isEmpty()
            && errors.get(errors.size() - 1).getPosition().getLine() == pos.getLine()) {
            return;
        }
        errors.add(ParseError.error(pos, Strings.apply(message, params)));
        if (errors.size() > MAX_ERRORS) {
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("More than %s errors occurred. Aborting.", MAX_ERRORS)
                            .handle();
        }
    }

    /**
     * Records a warning for the given position.
     *
     * @param pos     the position where the warning occured
     * @param message the message to show
     * @param params  the formatting parameters applied to the message
     */
    public void warning(Position pos, String message, Object... params) {
        errors.add(ParseError.warning(pos, Strings.apply(message, params)));
        if (errors.size() > MAX_ERRORS) {
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("More than %s errors occurred. Aborting.", MAX_ERRORS)
                            .handle();
        }
    }

    /**
     * Returns all errors and warnings collected during the compilation.
     *
     * @return a list of all errors and warnings
     */
    public List<ParseError> getErrors() {
        return Collections.unmodifiableList(errors);
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
        CompilationContext compilationContext = this;
        StringBuilder loop = new StringBuilder();
        while (compilationContext != null) {
            if (loop.length() > 0) {
                loop.append("; ");
            }
            loop.append(compilationContext.getTemplate().getName());
            if (Strings.areEqual(name, compilationContext.template.getName())) {
                ParseError parseError =
                        ParseError.error(position, Strings.apply("Recursive template dependency: %s", loop));
                throw CompileException.create(compilationContext.template,
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
                                  Function<String, Expression> arguments,
                                  Map<String, Emitter> blocks) {
        outputTemplateDeprecationWarning(position, template);
        InvokeTemplateEmitter emitter = new InvokeTemplateEmitter(position, template.getName());

        if (!template.getArguments().isEmpty()) {
            Expression[] args = collectArgumentsForInvoke(position, template, arguments);
            emitter.setArguments(args);
        }

        emitter.setBlocks(blocks);
        return emitter;
    }

    private Expression[] collectArgumentsForInvoke(Position position,
                                                   Template template,
                                                   Function<String, Expression> arguments) {
        Expression[] args = new Expression[template.getArguments().size()];
        int index = 0;
        for (TemplateArgument arg : template.getArguments()) {
            Expression argumentExpression = arguments.apply(arg.getName());
            if (argumentExpression != null) {
                if (!Tagliatelle.isAssignableTo(argumentExpression.getType(), arg.getType())) {
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
            if (this.parent == null && (Sirius.isDev() || Sirius.isStartedAsTest())) {
                error(position, "The attribute '%s' is deprecated: %s", arg.getName(), arg.getDeprecationWarning());
            } else {
                warning(position, "The attribute '%s' is deprecated: %s", arg.getName(), arg.getDeprecationWarning());
            }
        }
    }

    private void outputTemplateDeprecationWarning(Position position, Template template) {
        if (Strings.isFilled(template.getPragma(PRAGMA_DEPRECATED))) {
            if (this.parent == null && (Sirius.isDev() || Sirius.isStartedAsTest())) {
                error(position,
                      "The template '%s' is deprecated: %s",
                      template.getShortName(),
                      template.getPragma(PRAGMA_DEPRECATED));
            } else {
                warning(position,
                        "The template '%s' is deprecated: %s",
                        template.getShortName(),
                        template.getPragma(PRAGMA_DEPRECATED));
            }
        }
    }

    /**
     * Resolve a string (name) into a java class and reports an error if no matching class was found.
     * <p>
     * Next to the classic <tt>Class.forName</tt> this also supports aliases like <tt>String</tt> for
     * <tt>java.lang.String</tt>.
     *
     * @param position the position for error reporting
     * @param typeName the type name to resolve
     * @return a Java class for the given type name
     * @see Tagliatelle#getClassAliases()
     */
    public Class<?> resolveClass(Position position, String typeName) {
        Class<?> result = tryResolveClass(typeName).orElse(null);
        if (result == null) {
            error(position, "Cannot resolve '%s' to a Java class", typeName);
            result = void.class;
        }
        return result;
    }

    /**
     * Tries to resolve a string (name) into a java class.
     * <p>
     * Next to the classic <tt>Class.forName</tt> this also supports aliases like <tt>String</tt> for
     * <tt>java.lang.String</tt>.
     *
     * @param typeName the type name to resolve
     * @return a Java class for the given type name wrapped as optional or an empty optional, if no matching class was
     * found.
     * @see Tagliatelle#getClassAliases()
     */
    public Optional<Class<?>> tryResolveClass(String typeName) {
        Class<?> result = engine.getClassAliases().get(typeName);
        if (result != null) {
            return Optional.of(result);
        }

        try {
            return Optional.of(Class.forName(typeName));
        } catch (ClassNotFoundException e) {
            Exceptions.ignore(e);
            return Optional.empty();
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(template.getName());
        sb.append("\n\n");

        if (!globals.isEmpty()) {
            sb.append("Globals\n-------\n");
            for (Tuple<String, Class<?>> var : globals) {
                sb.append(var.getFirst());
                sb.append(": ");
                sb.append(var.getSecond());
                sb.append("\n");
            }
            sb.append("\n");
        }

        if (!stack.isEmpty()) {
            sb.append("Stack\n-----\n");
            for (StackLocation var : stack) {
                sb.append(var.name);
                sb.append(": ");
                sb.append(var.type);
                sb.append(" (");
                sb.append(var.stackIndex);
                sb.append(")\n");
            }
            sb.append("\n");
        }

        if (!errors.isEmpty()) {
            sb.append("Errors / Warnings\n-----------------\n");
            for (ParseError error : errors) {
                sb.append(error);
                sb.append("\n");
            }
        }

        return sb.toString();
    }
}
