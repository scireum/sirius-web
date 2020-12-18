/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Responsible for keeping track which variables are currently visible.
 * <p>
 * This also assigns each variable an unique index as the interperter later on only stores them in a simple list.
 */
public class VariableScoper {

    /**
     * Lists all keywords which cannot be used as variable name.
     */
    private static final Set<String> RESERVED_NAMES = Collections.unmodifiableSet(Stream.of(Parser.KEYWORD_TRUE,
                                                                                            Parser.KEYWORD_FALSE,
                                                                                            Parser.KEYWORD_LET,
                                                                                            Parser.KEYWORD_NULL)
                                                                                        .collect(Collectors.toSet()));

    /**
     * Represents a variable.
     */
    public static class Variable {
        private String name;
        private int localIndex;
        private Class<?> type;
        private Type genericType;

        protected Variable(String name, int localIndex, Class<?> type, Type genericType) {
            this.name = name;
            this.localIndex = localIndex;
            this.type = type;
            this.genericType = genericType;
        }

        /**
         * Returns the name of the variable.
         *
         * @return the name of the variable
         */
        public String getName() {
            return name;
        }

        /**
         * Returns the index of the variable
         *
         * @return the index of the variable which is later managed via the environment
         */
        public int getLocalIndex() {
            return localIndex;
        }

        /**
         * Returns the type of the variable.
         *
         * @return the type of the variable
         */
        public Class<?> getType() {
            return type;
        }

        public Type getGenericType() {
            return genericType;
        }

        @Override
        public String toString() {
            return Strings.apply("%s (%s - Index: %s)", name, type, localIndex);
        }
    }

    /**
     * Represents a scope (like a block) which can wrap a set of variables.
     */
    public class Scope {
        private int maxIndex = 0;

        protected Scope() {
            this.maxIndex = variables.size();
        }

        /**
         * Removes the scope from the variable scope which essentially hides all variables which were
         * defined in this scope.
         */
        public void pop() {
            while (variables.size() > maxIndex) {
                variables.remove(variables.size() - 1);
            }
        }

        @Override
        public String toString() {
            return "Scope{ maxIndex=" + maxIndex + "}";
        }
    }

    private CompilationContext compilationContext;
    private List<Variable> variables = new ArrayList<>();
    private int maxVariables = 0;
    private List<Integer> scopes = new ArrayList<>();

    /**
     * Creates a new scoper for the given context
     *
     * @param compilationContext the context used to report errors to
     */
    public VariableScoper(CompilationContext compilationContext) {
        this.compilationContext = compilationContext;
    }

    /**
     * Tries to resolve the given variable.
     *
     * @param name the name to resolve
     * @return the variable wrapped as optional or an empty optional if the variable is unknown. Note if there
     * are several variables with the same name, the last defined which is currently visible is the one used.
     */
    public Optional<Variable> resolve(String name) {
        if (Strings.isEmpty(name)) {
            return Optional.empty();
        }
        for (int i = variables.size() - 1; i >= 0; i--) {
            Variable variable = variables.get(i);
            if (Strings.areEqual(variable.getName(), name)) {
                return Optional.of(variable);
            }
        }

        return Optional.empty();
    }

    /**
     * Pushes a new scope on the stack.
     * <p>
     * Use {@link Scope#pop()} to remove it once he scope / block is closed.
     *
     * @return the newly created scope
     */
    public Scope pushScope() {
        return new Scope();
    }

    /**
     * Defines a new variable.
     *
     * @param position    the position in the source code
     * @param name        the name of the variable
     * @param type        the type of the variable
     * @param genericType the generic type of the variable
     * @return the newly created variable
     */
    public Variable defineVariable(Position position, String name, Class<?> type, Type genericType) {
        if (RESERVED_NAMES.contains(name)) {
            compilationContext.error(position, "%s is a reserved name.", name);
        }
        Variable variable = new Variable(name, maxVariables++, type, genericType);
        variables.add(variable);
        return variable;
    }

    /**
     * Returns the total number of variables.
     *
     * @return the total number of variables which have to be managed by the environment
     */
    public int getMaxVariables() {
        return maxVariables;
    }
}
