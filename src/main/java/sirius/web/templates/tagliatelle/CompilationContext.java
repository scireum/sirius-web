/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle;

import parsii.tokenizer.Char;
import parsii.tokenizer.ParseError;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.web.templates.tagliatelle.tags.InvokeTagHandler;
import sirius.web.templates.tagliatelle.tags.TagArg;
import sirius.web.templates.tagliatelle.tags.TagBlock;
import sirius.web.templates.tagliatelle.tags.TagHandler;
import sirius.web.templates.tagliatelle.tags.TagIf;
import sirius.web.templates.tagliatelle.tags.TagPragma;
import sirius.web.templates.tagliatelle.tags.TagRender;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Created by aha on 11.05.17.
 */
public class CompilationContext {
    private final Engine engine;
    private List<Tuple<String, Class<?>>> stack = new ArrayList<>();
    private List<Tuple<String, Class<?>>> globals;
    private List<ParseError> errors = new ArrayList<>();
    private boolean foundErrors = false;
    private boolean foundWarnings = false;

    protected CompilationContext(Engine engine) {
        this.engine = engine;
this.globals = engine.getGlobalVariables();
    }

    public void push(String name, Class<?> type) {
        if (globals.stream().map(Tuple::getFirst).anyMatch(otherName -> Strings.areEqual(name, otherName))) {
            //TODO warn!
        }
        if (stack.stream().map(Tuple::getFirst).anyMatch(otherName -> Strings.areEqual(name, otherName))) {
            //TODO warn!
        }
        stack.add(Tuple.create(name, type));
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

    public TagHandler findTagHandler(String tagName) {
        if (tagName.equals("i:if")) {
            return new TagIf();
        } else if (tagName.equals("i:block")) {
            return new TagBlock();
        } else if (tagName.equals("i:arg")) {
            return new TagArg();
        } else if (tagName.equals("i:pragma")) {
            return new TagPragma();
        } else if (tagName.equals("i:render")) {
            return new TagRender();
        }

        if (tagName.contains(":")) {
            try {
                Template template = engine.resolveTag(tagName);
                return new InvokeTagHandler(template);
            } catch (CompileException e) {
                //TODO
                return null;
            }
        }

        return null;

    }
}
