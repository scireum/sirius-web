/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import parsii.tokenizer.Char;
import parsii.tokenizer.ParseError;
import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.tagliatelle.emitter.BlockEmitter;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.InlineTemplateEmitter;
import sirius.tagliatelle.emitter.InvokeTemplateEmitter;
import sirius.tagliatelle.emitter.PushLocalEmitter;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ReadLocal;
import sirius.tagliatelle.tags.InvokeTagHandler;
import sirius.tagliatelle.tags.TagHandler;
import sirius.tagliatelle.tags.TagHandlerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Created by aha on 11.05.17.
 */
public class CompilationContext {

    private final Template template;
    private final Engine engine;
    private int stackDepth = 0;
    private List<Tuple<String, Class<?>>> stack = new ArrayList<>();
    private List<Tuple<String, Class<?>>> globals;
    private List<ParseError> errors = new ArrayList<>();
    private boolean foundErrors = false;
    private boolean foundWarnings = false;

    @Part
    private static GlobalContext ctx;

    protected CompilationContext(Engine engine, Template template) {
        this.engine = engine;
        this.globals = engine.getGlobalVariables();
        this.template = template;
    }

    public int push(String name, Class<?> type) {
        if (globals.stream().map(Tuple::getFirst).anyMatch(otherName -> Strings.areEqual(name, otherName))) {
            //TODO warn!
        }
        if (stack.stream().map(Tuple::getFirst).anyMatch(otherName -> Strings.areEqual(name, otherName))) {
            //TODO warn!
        }
        stack.add(Tuple.create(name, type));
        if (stack.size() > stackDepth) {
            stackDepth = stack.size();
        }

        return stack.size() - 1;
    }

    public void pop() {
        if (!stack.isEmpty()) {
            stack.remove(stack.size() - 1);
        } else {
            //TODO
        }
    }

    public Optional<Tuple<Class<?>, Integer>> findLocal(String variableName) {
        for (int i = stack.size() - 1; i >= 0; i--) {
            Tuple<String, Class<?>> localVariable = stack.get(i);
            if (Strings.areEqual(variableName, localVariable.getFirst())) {
                return Optional.of(Tuple.create(localVariable.getSecond(), i));
            }
        }

        return Optional.empty();
    }

    public Optional<Tuple<Class<?>, Integer>> findGlobal(String variableName) {
        for (int i = globals.size() - 1; i >= 0; i--) {
            Tuple<String, Class<?>> globalVariable = globals.get(i);
            if (Strings.areEqual(variableName, globalVariable.getFirst())) {
                return Optional.of(Tuple.create(globalVariable.getSecond(), i));
            }
        }

        return Optional.empty();
    }

    public void error(Char pos, String message, Object... params) {
        foundErrors = true;
        errors.add(ParseError.error(pos, Strings.apply(message, params)));
    }

    public void warning(Char pos, String message, Object... params) {
        foundWarnings = true;
        errors.add(ParseError.warning(pos, Strings.apply(message, params)));
    }

    public List<ParseError> getErrors() {
        return errors;
    }

    public boolean hasErrors() {
        return foundErrors;
    }

    public boolean hasWarnings() {
        return foundWarnings;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
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

    public TagHandler findTagHandler(String tagName) throws CompileException {
        if (!tagName.contains(":")) {
            return null;
        }

        if (tagName.startsWith("i:")) {
            TagHandlerFactory factory = ctx.getPart(tagName, TagHandlerFactory.class);
            if (factory != null) {
                return factory.createHandler();
            }
        }

        return engine.resolveTag(tagName).map(InvokeTagHandler::new).orElse(null);
    }

    public Emitter invokeTemplate(Position startOfTag,
                                  Template template,
                                  Function<String, Expression> arguments,
                                  Map<String, Emitter> blocks) {
        InvokeTemplateEmitter emitter = new InvokeTemplateEmitter(startOfTag, template.getName());

        if (!template.getArguments().isEmpty()) {
            Expression[] args = new Expression[template.getArguments().size()];
            int index = 0;
            for (TemplateArgument arg : template.getArguments()) {
                args[index] = arguments.apply(arg.getName());
                index++;
            }
            emitter.setArguments(args);
        }

        emitter.setBlocks(blocks);
        return emitter;
    }

    public Emitter inlineTemplate(Position startOfTag,
                                  Template template,
                                  Function<String, Expression> arguments,
                                  Function<String, Emitter> blocks) {
        List<PushLocalEmitter> temps = new ArrayList<>();
        Emitter copy = template.getEmitter().copy();
        if (!template.getArguments().isEmpty()) {
            List<Expression> defaultArgs = template.getArguments()
                                                   .stream()
                                                   .map(TemplateArgument::getDefaultValue)
                                                   .map(e -> e == null ? null : e.copy())
                                                   .collect(Collectors.toList());
            int index = 0;
            for (TemplateArgument arg : template.getArguments()) {
                Expression value = arguments.apply(arg.getName());
                if (value == null) {
                    value = defaultArgs.get(index);
                    if (value == null) {
                        //TODO error
                    }
                }
                if (!value.isConstant() && !(value instanceof ReadLocal)) {
                    int temporaryIndex = push(null, arg.getType());
                    temps.add(new PushLocalEmitter(startOfTag, temporaryIndex, value));
                    value = new ReadLocal(arg.getType(), temporaryIndex);
                }

                int currentIndex = index;
                Expression currentValue = value;
                for (int i = index + 1; i < defaultArgs.size(); i++) {
                    defaultArgs.set(index, defaultArgs.get(index).visit(e -> {
                        if (e instanceof ReadLocal) {
                            if (((ReadLocal) e).getIndex() == currentIndex) {
                                return currentValue;
                            }
                        }

                        return e;
                    }));
                }
                copy.visitExpressions(e -> {
                    if (e instanceof ReadLocal) {
                        if (((ReadLocal) e).getIndex() == currentIndex) {
                            return currentValue;
                        }
                    }

                    return e;
                });

                index++;
            }
        }

        copy = copy.visit(e -> {
            if (e instanceof BlockEmitter) {
                BlockEmitter blockEmitter = (BlockEmitter) e;
                Emitter result = blocks.apply(blockEmitter.getName());
                if (result != null) {
                    result = new InlineTemplateEmitter(result.getStartOfBlock(), getTemplate(), result);
                } else {
                    result = blockEmitter.getAlternative();
                }
                if (result == null) {
                    result = ConstantEmitter.EMPTY;
                }

                return result;
            }

            return e;
        });

        CompositeEmitter ce = new CompositeEmitter(copy.getStartOfBlock());
        temps.forEach(ce::addChild);
        temps.forEach(e -> pop());

        ce.addChild(copy);
        return new InlineTemplateEmitter(startOfTag, template, ce.reduce());
    }

    public Template getTemplate() {
        return template;
    }

    public int getStackDepth() {
        return stackDepth;
    }
}
