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
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.BlockEmitter;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.InlineTemplateEmitter;
import sirius.tagliatelle.emitter.InvokeTemplateEmitter;
import sirius.tagliatelle.emitter.PushEmitter;
import sirius.tagliatelle.emitter.PushLocalEmitter;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.expression.ReadLocal;
import sirius.tagliatelle.tags.InvokeTagHandler;
import sirius.tagliatelle.tags.TagHandler;
import sirius.tagliatelle.tags.TagHandlerFactory;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Provides a context for compiling a <tt>.pasta</tt> file.
 * <p>
 * The main responsibility is to collect all errors or warnings and to provide the stack context (the names of all
 * arguments and local variables).
 */
public class CompilationContext {

    private static final int INLINE_LOCALS_SHIFT_OFFSET = 1000000;

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
    private List<Tuple<String, Class<?>>> stack = new ArrayList<>();

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
     * Pushes a local variable on the stack, so that it can be resolved later.
     *
     * @param position the position where the local is defined - mainly used for error reporting
     * @param name     the name of the variable
     * @param type     the type of the variable
     * @return the stack index of the variable
     */
    public int push(Position position, @Nullable String name, @Nullable Class<?> type) {
        if (Strings.isFilled(name)) {
            if (globals.stream().map(Tuple::getFirst).anyMatch(otherName -> Strings.areEqual(name, otherName))) {
                warning(position, "Argument or local variable hides a global: %s", name);
            }
            if (stack.stream().map(Tuple::getFirst).anyMatch(otherName -> Strings.areEqual(name, otherName))) {
                warning(position, "Argument or local variable hides another one: %s", name);
            }
        }
        stack.add(Tuple.create(name, type));
        if (stack.size() > stackDepth) {
            stackDepth = stack.size();
        }

        return stack.size() - 1;
    }

    /**
     * Pops a local off the stack.
     *
     * @param position the position which caused the pop - mainly used for error reporting
     */
    public void pop(Position position) {
        if (!stack.isEmpty()) {
            stack.remove(stack.size() - 1);
        } else {
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
     * Tries to resolve a variable name into a stack location.
     *
     * @param variableName the name of the variable
     * @return the type and the stack location of the variable or an empty optional, if no such variable exists
     */
    public Optional<Tuple<Class<?>, Integer>> findLocal(String variableName) {
        for (int i = stack.size() - 1; i >= 0; i--) {
            Tuple<String, Class<?>> localVariable = stack.get(i);
            if (Strings.areEqual(variableName, localVariable.getFirst())) {
                return Optional.of(Tuple.create(localVariable.getSecond(), i));
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
        errors.add(ParseError.error(pos, Strings.apply(message, params)));
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
    }

    /**
     * Returns all errors and warnings collected during the compilation.
     *
     * @return a list of all errors and warnings
     */
    public List<ParseError> getErrors() {
        return errors;
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
            }
        }

        try {
            return resolveTemplate(position, engine.resolveTagName(tagName)).map(InvokeTagHandler::new).orElse(null);
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
            args[index] = arguments.apply(arg.getName());
            if (args[index] != null && !arg.getType().isAssignableFrom(args[index].getType())) {
                error(position,
                      "Incompatible attribute types. '%s' expects %s for '%s', but %s was given.",
                      template.getName(),
                      arg.getType(),
                      arg.getName(),
                      args[index].getType());
            }
            index++;
        }
        return args;
    }

    /**
     * Inlines the requested template into the current one.
     * <p>
     * Note that the current state and content of the given template is used and that no further modifications of
     * the referenced template are reflected (until the current template is recompiled). So this should only be used
     * for static templates, not for ones modified by the user.
     * <p>
     * Note however, that aggressive optimizations are enabled by this approach as all constant arguments are
     * propagated to the referenced template. Also all emitter blocks passed to the template are resolved and propagated
     * during compilation.
     * <p>
     * As constant expressions and constant conditional emitters are optimized during the last phase of the compilation,
     * this might yield in a much faster and more condense final template, especially if many configuration flags,
     * as e.g. in the wondergem components, are present.
     *
     * @param startOfTag the position where the invokation took place
     * @param template   the template to call
     * @param arguments  the arguments passed to the template
     * @param blocks     the emitter blocks passed to the template
     * @return an emitter which is basically a copy of the referenced template, where all arguments and emitter blocks
     * haven been propagated so that the final and global optimization phase will be very effective
     */
    public Emitter inlineTemplate(Position startOfTag,
                                  Template template,
                                  Function<String, Expression> arguments,
                                  Function<String, Emitter> blocks) {
        Emitter copy = template.getEmitter().copy();

        copy.visitExpressions(pos -> this::shiftLocalReads);
        copy = propagateArgumentsForInline(startOfTag, template, arguments, copy);
        copy = transferLocals(copy);

        copy = propagateBlocksForInline(blocks, copy);

        copy = copy.reduce();

        return new InlineTemplateEmitter(startOfTag, template, copy);
    }

    /**
     * As we have to map locals and arguments from the template being inlined to the stack of the caller,
     * we have to re-assign all stack locations.
     * <p>
     * However, during the process stack locations might collide, therefore we shift all stack locations
     * by a large number and down shift them one by another once they are processed.
     *
     * @param expr the expression to process
     * @return the updated expression
     */
    private Expression shiftLocalReads(Expression expr) {
        if (expr instanceof ReadLocal) {
            return new ReadLocal(expr.getType(), ((ReadLocal) expr).getIndex() + INLINE_LOCALS_SHIFT_OFFSET);
        } else {
            return expr;
        }
    }

    /**
     * Propagates all arguments into the locations where these are referenced.
     * <p>
     * Basically this method scans for all argument references ({@link ReadLocal} and replaces them with the given
     * expression. This is also done for the default arguments (or copies of them actually), as these themself can be
     * expressions that reference previous arguments.
     * <p>
     * If a non trivial argument (neither a constant nor a local variable reference) is found,
     * we cannot replace every occurence of this argument with the expression each time,
     * as e.g. a method call might have side effects which must only occur once.
     * Therefore a temporary is defined for each such argument, then assigned via a
     * PushLocalEmitter and subsequently referenced via a plain ReadLocal.
     *
     * @param position  the position of the invokation for error reporting
     * @param template  the template being inlined
     * @param arguments the argument expression supplied
     * @param copy      the copied contents of the template being inline
     * @return an emitter where all argument references have been replaced by the argument expressions
     */
    private Emitter propagateArgumentsForInline(Position position,
                                                Template template,
                                                Function<String, Expression> arguments,
                                                Emitter copy) {
        List<Expression> defaultArgs = template.getArguments()
                                               .stream()
                                               .map(TemplateArgument::getDefaultValue)
                                               .map(e -> e == null ? null : shiftLocalReads(e.copy()))
                                               .collect(Collectors.toList());
        List<PushLocalEmitter> temps = new ArrayList<>();

        AtomicInteger index = new AtomicInteger(0);
        for (TemplateArgument arg : template.getArguments()) {
            Expression value = determineArgumentExpression(position, arg, index.get(), arguments, defaultArgs, temps);
            ExpressionVisitor replaceArgumentVisitor = createReplaceArgumentVisitor(index.get(), value);
            for (int i = index.get() + 1; i < defaultArgs.size(); i++) {
                if (defaultArgs.get(i) != null) {
                    defaultArgs.set(i, defaultArgs.get(i).propagateVisitor(replaceArgumentVisitor));
                }
            }

            index.incrementAndGet();
        }

        if (temps.isEmpty()) {
            return copy;
        }

        CompositeEmitter result = new CompositeEmitter(copy.getStartOfBlock());
        temps.forEach(e -> {
            result.addChild(e);
            pop(position);
        });

        result.addChild(copy);
        return result;
    }

    /**
     * Fetches the expression given for the given argument.
     * <p>
     * First we try to simply use the argument value. If no value is given, we resort to the default expression.
     * <p>
     * Then we ensure that the expression can be evaluated several times without side effects. If not, a temporary
     * location is assigned so that the expression is only evaluated once and the value then kept on the stack.
     *
     * @param position    the position of the call for error reporting.
     * @param arg         the argument to fill / proparage
     * @param index       the index within the argument list
     * @param arguments   the provider which provides the argument expressions
     * @param defaultArgs the default arguments (we keep a copy here, as replacements also have to be performed here)
     * @param temps       the list of temporary stack locations
     * @return a final expression which can be used to relace each argument reference
     */
    private Expression determineArgumentExpression(Position position,
                                                   TemplateArgument arg,
                                                   int index,
                                                   Function<String, Expression> arguments,
                                                   List<Expression> defaultArgs,
                                                   List<PushLocalEmitter> temps) {
        Expression value = arguments.apply(arg.getName());
        if (value == null) {
            value = defaultArgs.get(index);
            if (value == null) {
                error(position, "No argument value (and no default) was provided for: %s", arg.getName());
                value = ConstantNull.NULL;
            }
        }

        if (!value.isConstant() && !(value instanceof ReadLocal)) {
            int temporaryIndex = push(position, null, arg.getType());
            temps.add(new PushLocalEmitter(position, temporaryIndex, value));
            value = new ReadLocal(arg.getType(), temporaryIndex);
        }
        return value;
    }

    /**
     * Creates a visitor which replaces a reference to the given argument with the given value.
     * <p>
     * This also takes care of the stack locations created by {@link #shiftLocalReads(Expression)}.
     *
     * @param currentIndex the argument (local) index to replace
     * @param currentValue the replacement expression
     * @return a visitor performing the replacement task
     */
    private ExpressionVisitor createReplaceArgumentVisitor(int currentIndex, Expression currentValue) {
        return e -> {
            if (e instanceof ReadLocal) {
                if (((ReadLocal) e).getIndex() == currentIndex + INLINE_LOCALS_SHIFT_OFFSET) {
                    return currentValue;
                }
            }

            return e;
        };
    }

    /**
     * Moves local variables from the template stack to the callers stack.
     * <p>
     * This will essentially find all {@link PushEmitter push emitters} and assign a new stack location and update all
     * matching local reads.
     *
     * @param copy the emitters to process
     * @return the processed emitters
     */
    private Emitter transferLocals(Emitter copy) {
        AtomicInteger numberOfLocals = new AtomicInteger(0);
        Emitter result = copy.propagateVisitor(emitter -> {
            if (emitter instanceof PushEmitter) {
                PushEmitter pushEmitter = (PushEmitter) emitter;
                int newStackLocation = push(emitter.getStartOfBlock(), null, null);
                numberOfLocals.incrementAndGet();
                updateLocalReads(copy, pushEmitter.getLocalIndex(), newStackLocation);
                pushEmitter.setLocalIndex(newStackLocation);
            }

            return emitter;
        });

        // Pop the locally transferred variables off the stack
        while (numberOfLocals.getAndDecrement() > 0) {
            pop(copy.getStartOfBlock());
        }

        return result;
    }

    /**
     * Updates all {@link ReadLocal read locals} which access the old stack location to access the new stack location.
     * <p>
     * This also takes care of the artificial stack locations created by {@link #shiftLocalReads(Expression)}.
     *
     * @param copy             the emitter tree to process
     * @param oldStackLocation the old location to replace
     * @param newStackLocation the location on the new stack
     */
    private void updateLocalReads(Emitter copy, int oldStackLocation, int newStackLocation) {
        copy.visitExpressions(pos -> expr -> {
            if (expr instanceof ReadLocal
                && ((ReadLocal) expr).getIndex() == oldStackLocation + INLINE_LOCALS_SHIFT_OFFSET) {
                return new ReadLocal(expr.getType(), newStackLocation);
            } else {
                return expr;
            }
        });
    }

    /**
     * Propagates blocks for an inlined template.
     * <p>
     * As all positions where the blocks are used are known in advance, we directly replace the respective {@link
     * BlockEmitter} with the given blocks.
     * <p>
     * For complex blocks, we wrap them in an {@link InvokeTemplateEmitter}, simple (constant) ones don't need this
     * trick as we do not need to maintain the render stack as no render error can occur here.
     *
     * @param blocks the map of blocks available
     * @param copy   the copied template content
     * @return the template contant where all block references haven been replaced
     */
    private Emitter propagateBlocksForInline(Function<String, Emitter> blocks, Emitter copy) {
        return copy.propagateVisitor(e -> {
            if (e instanceof BlockEmitter) {
                BlockEmitter blockEmitter = (BlockEmitter) e;
                return replaceBlockReference(blocks, blockEmitter);
            }

            return e;
        });
    }

    /**
     * Determines the effective replacement block for a block reference.
     *
     * @param blocks       all blocks available
     * @param blockEmitter the block emitter referencing a block
     * @return the effective replacement for the referencing block
     */
    private Emitter replaceBlockReference(Function<String, Emitter> blocks, BlockEmitter blockEmitter) {
        Emitter result = blocks.apply(blockEmitter.getName());
        if (result != null) {
            return new InlineTemplateEmitter(result.getStartOfBlock(), getTemplate(), result);
        }

        if (blockEmitter.getAlternative() == null) {
            return ConstantEmitter.EMPTY;
        }

        return blockEmitter.getAlternative();
    }

    /**
     * Tries to resolve a string (name) into a java class.
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
        Class<?> result = engine.getClassAliases().get(typeName);
        if (result != null) {
            return result;
        }

        try {
            return Class.forName(typeName);
        } catch (ClassNotFoundException e) {
            Exceptions.ignore(e);
            error(position, "Cannot resolve '%s' to a Java class", typeName);
            return void.class;
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
            for (Tuple<String, Class<?>> var : stack) {
                sb.append(var.getFirst());
                sb.append(": ");
                sb.append(var.getSecond());
                sb.append("\n");
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
