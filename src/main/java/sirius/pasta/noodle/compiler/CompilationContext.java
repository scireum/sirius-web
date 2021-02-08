/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import parsii.tokenizer.ParseError;
import parsii.tokenizer.Position;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.health.Exceptions;
import sirius.pasta.Pasta;
import sirius.pasta.noodle.ClassAliasProvider;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Provides a context for compiling a <tt>Noodle</tt> script.
 * <p>
 * The main responsibility is to collect all errors or warnings and to provide the stack context (the names of all
 * arguments and local variables).
 */
public class CompilationContext {

    /**
     * Defines the maximal number of errors to collect.
     * <p>
     * The compilers itself are quite optimistic and try to recover from errors to provide as much information
     * as possible. However, as sometimes one error leads to another, we use this limit here to determine when
     * to abort (reject any further errors).
     */
    private static final int MAX_ERRORS = 250;

    private final SourceCodeInfo sourceCodeInfo;

    /**
     * Used to keep track of all shared variables.
     */
    private final VariableScoper variableScoper = new VariableScoper(this);

    /**
     * Determines if the security sandbox is enabled.
     * <p>
     * The sandbox is used to safely execute code provided by users of the system.
     */
    private boolean enableSandbox;

    /**
     * Used to conrol which errors are really reported.
     * <p>
     * The {@link Parser} controls this in order to skip over subsequent errors.
     */
    private boolean skipErrors;

    /**
     * Contains a list of errors and warnings which occured during compilation.
     */
    private final List<ParseError> errors = new ArrayList<>();

    /**
     * Keeps track of all imported classes.
     */
    private final Map<String, Class<?>> importedClasses = new HashMap<>();

    @PriorityParts(ClassAliasProvider.class)
    private static List<ClassAliasProvider> aliasProviders;

    /**
     * Contains all aliases collected via {@link ClassAliasProvider alias providers}.
     */
    private static Map<String, Class<?>> aliases;

    /**
     * Creates a new compilation context for the given source code.
     *
     * @param sourceCodeInfo the source code to copmpile
     */
    public CompilationContext(SourceCodeInfo sourceCodeInfo) {
        this.sourceCodeInfo = sourceCodeInfo;
    }

    /**
     * Provides access to the variable scoper of this context.
     *
     * @return the variable scoper used by this context
     */
    public VariableScoper getVariableScoper() {
        return variableScoper;
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
        if (skipErrors) {
            return;
        }
        if (errors.size() < MAX_ERRORS) {
            errors.add(ParseError.error(pos, Strings.apply(message, params)));
        }
    }

    /**
     * Instructs the context to ignore all reported errors until {@link #reEnableErrors()} is called.
     */
    public void skipErrors() {
        if (!Sirius.isStartedAsTest()) {
            this.skipErrors = true;
        }
    }

    /**
     * Re-enables the reporting of errors.
     */
    public void reEnableErrors() {
        this.skipErrors = false;
    }

    /**
     * Records a warning for the given position.
     *
     * @param pos     the position where the warning occured
     * @param message the message to show
     * @param params  the formatting parameters applied to the message
     */
    public void warning(Position pos, String message, Object... params) {
        if (skipErrors) {
            return;
        }

        if (errors.size() < MAX_ERRORS) {
            errors.add(ParseError.warning(pos, Strings.apply(message, params)));
        }
    }

    /**
     * Returns all errors and warnings collected during the compilation.
     * <p>
     * Note that this returns the raw {@link ParseError parse errors}. Use {@link #processCollectedErrors()} which
     * automatically creates {@link CompileError compile errors} and also thows a {@link CompileException} if
     * necessarry.
     *
     * @return a list of all errors and warnings
     */
    public List<ParseError> getErrors() {
        return Collections.unmodifiableList(errors);
    }

    /**
     * Processes all collected errors and determines if a {@link CompileException} should be thrown.
     *
     * @return a list of warnings as {@link CompileError} including the relevant source lines
     * @throws CompileException in case one or more errors were collected while compiling the source
     */
    public List<CompileError> processCollectedErrors() throws CompileException {
        if (getErrors().isEmpty()) {
            return Collections.emptyList();
        }

        List<CompileError> compileErrors = getCompilationErrors();
        boolean errorFound = false;
        for (ParseError error : getErrors()) {
            if (Pasta.LOG.isFINE()) {
                Pasta.LOG.FINE("'%s': %s", sourceCodeInfo.getName(), error);
            }

            errorFound |= error.getSeverity() == ParseError.Severity.ERROR;
        }

        if (errorFound) {
            throw CompileException.create(sourceCodeInfo.getName(), sourceCodeInfo.getLocation(), compileErrors);
        }

        return compileErrors;
    }

    public List<CompileError> getCompilationErrors() {
        return getErrors().stream()
                          .map(parseError -> new CompileError(parseError,
                                                              sourceCodeInfo.fetchLine(parseError.getPosition()
                                                                                                 .getLine())))
                          .collect(Collectors.toList());
    }

    /**
     * Adds an import for a class name.
     *
     * @param position the location within the source code
     * @param alias    the simple class name
     * @param clazz    the class to actually import
     * @return the context itself for fluent method calls
     */
    public CompilationContext addImport(Position position, String alias, Class<?> clazz) {
        Class<?> offendingClass = getClassAliases().get(alias);
        if (offendingClass == null) {
            offendingClass = importedClasses.get(alias);
        }
        if (offendingClass != null) {
            if (offendingClass.equals(clazz)) {
                warning(position, "The class %s is already imported.");
            } else {
                error(position,
                      "Cannot import %s as '%s' as this alias is already used for %s",
                      clazz,
                      alias,
                      offendingClass);
                return this;
            }
        }

        importedClasses.put(alias, clazz);
        return this;
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
     * Tries to resolve a string (name) into a Java class.
     * <p>
     * Next to the classic <tt>Class.forName</tt> this also supports aliases like <tt>String</tt> for
     * <tt>java.lang.String</tt>.
     *
     * @param typeName the type name to resolve
     * @return a Java class for the given type name wrapped as optional or an empty optional, if no matching class was
     * found.
     */
    public Optional<Class<?>> tryResolveClass(String typeName) {
        Class<?> result = importedClasses.get(typeName);
        if (result != null) {
            return Optional.of(result);
        }

        result = getClassAliases().get(typeName);
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

    private static Map<String, Class<?>> getClassAliases() {
        if (aliases == null) {
            Map<String, Class<?>> aliasMap = new HashMap<>();
            aliasProviders.forEach(p -> p.collectAliases((name, type) -> addAlias(aliasMap, name, type)));
            aliasProviders.forEach(p -> p.collectOptionalAliases((name, type) -> addOptionalAlias(aliasMap,
                                                                                                  name,
                                                                                                  type)));
            aliases = aliasMap;
        }

        return Collections.unmodifiableMap(aliases);
    }

    private static void addAlias(Map<String, Class<?>> aliasMap, String name, Class<?> type) {
        if (aliasMap.containsKey(name)) {
            Pasta.LOG.WARN("Failed to register %s as class alias for %s - This is already used by: %s",
                           name,
                           type,
                           aliasMap.get(name));
        } else {
            aliasMap.put(name, type);
        }
    }

    private static void addOptionalAlias(Map<String, Class<?>> aliasMap, String name, Class<?> type) {
        if (!aliasMap.containsKey(name)) {
            aliasMap.put(name, type);
        }
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
     * Returns tha matching "boxed" class for the given class.
     * <p>
     * This will return the boxed type for a primitive class like <tt>Integer</tt> for <tt>int</tt>.
     *
     * @param type the class to box
     * @return the boxed version of the given class or the class itself if it wasn't a primitive
     */
    public static Class<?> autoboxClass(Class<?> type) {
        if (type == int.class) {
            return Integer.class;
        }
        if (type == long.class) {
            return Long.class;
        }
        if (type == double.class) {
            return Double.class;
        }
        if (type == boolean.class) {
            return Boolean.class;
        }

        return type;
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

        Class<?> unboxedFrom = autoboxClass(from);
        Class<?> unboxedTo = autoboxClass(to);

        if (unboxedTo.isAssignableFrom(unboxedFrom)) {
            return true;
        }

        if (Long.class.equals(unboxedTo)) {
            return Integer.class.equals(unboxedFrom);
        }
        if (Double.class.equals(unboxedTo)) {
            return Integer.class.equals(unboxedFrom) || Long.class.equals(unboxedFrom);
        }

        return false;
    }

    /**
     * Finds the compatible number type for two given types.
     * <p>
     * E.g. if adding an <tt>int</tt> to a <tt>double</tt> the compatible type would be a <tt>double</tt> as
     * both values can be assigned to it.
     *
     * @param typeA the first type to check
     * @param typeB the second type to check
     * @return a type which is compatible with both given types
     */
    public static Class<?> coerceNumericTypes(Class<?> typeA, Class<?> typeB) {
        if (autoboxClass(typeA) == Double.class || autoboxClass(typeB) == Double.class) {
            return Double.class;
        }
        if (autoboxClass(typeA) == Long.class && autoboxClass(typeB) == Long.class) {
            return Long.class;
        }

        return Integer.class;
    }

    /**
     * Determines if the security sandbox is active or not.
     *
     * @return <tt>true</tt> if the sandbox should be active, <tt>false</tt> otherwise
     */
    public boolean isSandboxEnabled() {
        return enableSandbox;
    }

    /**
     * Enables the security sandbox.
     * <p>
     * The {@link sirius.pasta.noodle.sandbox.Sandbox} performs checks for each method call and macro invocation
     * and only permits to call accessible methods which are considered safe to be invoked by user supplied code.
     * <p>
     * Note that the sandbox is disabled by default and has to be enabled when compiling user supplied code.
     */
    public void enableSandbox() {
        this.enableSandbox = true;
    }

    /**
     * Disables the security sandbox.
     * <p>
     * Note that the sandbox is disabled by default so this method only needs to be invoked if the sandbox has
     * previously been enabled manually.
     */
    public void disableSandbox() {
        this.enableSandbox = false;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        if (!errors.isEmpty()) {
            result.append("Errors / Warnings\n-----------------\n");
            for (ParseError error : errors) {
                result.append(error);
                result.append("\n");
            }
        }

        return result.toString();
    }

    public SourceCodeInfo getSourceCodeInfo() {
        return sourceCodeInfo;
    }
}
