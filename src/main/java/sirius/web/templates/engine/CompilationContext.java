/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Created by aha on 11.05.17.
 */
public class CompilationContext {
    private List<Tuple<String, Class<?>>> stack = new ArrayList<>();
    private List<Tuple<String, Class<?>>> globals = new ArrayList<>();

    public void registerGlobal(String name, Class<?> type) {
        if (globals.stream().map(Tuple::getFirst).anyMatch(otherName -> Strings.areEqual(name, otherName))) {
            //TODO warn!
        }
        globals.add(Tuple.create(name, type));
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
}
