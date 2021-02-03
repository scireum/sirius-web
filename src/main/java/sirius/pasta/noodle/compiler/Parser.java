/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import parsii.tokenizer.Char;
import parsii.tokenizer.LookaheadReader;
import parsii.tokenizer.Position;
import sirius.kernel.commons.Reflection;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.PartCollection;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.transformers.Transformable;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.pasta.Pasta;
import sirius.pasta.noodle.InterpreterCall;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.ir.Assignment;
import sirius.pasta.noodle.compiler.ir.BinaryOperation;
import sirius.pasta.noodle.compiler.ir.BlockNode;
import sirius.pasta.noodle.compiler.ir.Conjunction;
import sirius.pasta.noodle.compiler.ir.Constant;
import sirius.pasta.noodle.compiler.ir.Disjunction;
import sirius.pasta.noodle.compiler.ir.InstanceOfCheck;
import sirius.pasta.noodle.compiler.ir.MacroCall;
import sirius.pasta.noodle.compiler.ir.MethodCall;
import sirius.pasta.noodle.compiler.ir.NativeCast;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.compiler.ir.PopField;
import sirius.pasta.noodle.compiler.ir.PushField;
import sirius.pasta.noodle.compiler.ir.PushTemporary;
import sirius.pasta.noodle.compiler.ir.RawClassLiteral;
import sirius.pasta.noodle.compiler.ir.TernaryOperation;
import sirius.pasta.noodle.compiler.ir.UnaryOperation;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Parses {@link Node expressions} used by <tt>Noodle</tt>.
 * <p>
 * The parser is invoked by the {@link NoodleCompiler} so there is most probably no need to call it directly.
 * It is implemented as a simple recursive descending parser.
 */
public class Parser extends InputProcessor {

    /**
     * Represents the keyword used to define a variable.
     */
    public static final String KEYWORD_LET = "let";

    /**
     * Represents the keyword used to declare a class literal (used as suffix as in Java).
     */
    public static final String KEYWORD_CLASS = "class";

    /**
     * Defines the special method which is either a cast or will invoke {@link Transformable#as(Class)} if the
     * self expression matches.
     */
    public static final String KEYWORD_METHOD_AS = "as";

    /**
     * Defines the special method which is either an instanceof check or will invoke {@link Transformable#is(Class)}
     * if the self expression matches.
     */
    public static final String KEYWORD_METHOD_IS = "is";

    /**
     * Represents <tt>true</tt>.
     */
    public static final String KEYWORD_TRUE = "true";

    /**
     * Represents <tt>false</tt>.
     */
    public static final String KEYWORD_FALSE = "false";

    /**
     * Represents <tt>null</tt>.
     */
    public static final String KEYWORD_NULL = "null";

    /**
     * Determines if whitespace characters can be skipped.
     * <p>
     * As expessions which are inlined in the template code commonly stop at a whitespace, we must not always
     * skip them but rather abort parsing. On the other hand there are many locations, like method calls, where
     * we can safely skip whitespaces as the one of the expression is determined by a ')' in this case.
     * <p>
     * Therefore we store the current mode of operation in this field.
     */
    private boolean canSkipWhitespace = false;

    @Parts(LegacyGlobalsHandler.class)
    private static PartCollection<LegacyGlobalsHandler> legacyGlobalsHandlers;

    /**
     * Creates a new parser for the given input and context.
     *
     * @param context the context to use
     */
    protected Parser(CompilationContext context, LookaheadReader reader) {
        super(context, reader);
    }

    protected Node block() {
        canSkipWhitespace = true;
        VariableScoper.Scope scope = context.getVariableScoper().pushScope();
        BlockNode block = new BlockNode(reader.current());
        while (!reader.current().isEndOfInput() && !reader.current().is('}')) {
            block.addStatement(statement());
            skipWhitespaces();
            if (consumeExpectedCharacter(';')) {
                context.reEnableErrors();
            }
            skipWhitespaces();
        }
        scope.pop();
        context.reEnableErrors();

        return block;
    }

    protected Node statement() {
        skipWhitespaces();
        if (isAtKeyword(KEYWORD_LET)) {
            return assignment();
        }

        Node expression = parseExpression();
        skipWhitespaces();
        if (reader.current().is('=')) {
            if (expression instanceof PushField) {
                PushField pushField = (PushField) expression;
                Field field = pushField.getField();
                PopField popField = new PopField(reader.consume(), field);
                popField.setSelfExpression(pushField.getSelfExpression());
                popField.setValueExpression(parseExpression());

                if (Modifier.isFinal(field.getModifiers())) {
                    context.error(popField.getPosition(),
                                  "The field '%s' of '%s' is final and cannot be assigned with a value.",
                                  field.getName(),
                                  field.getDeclaringClass().getName());
                }

                return popField;
            }
        }

        return expression;
    }

    @Nonnull
    private Node assignment() {
        reader.consume(3);
        skipWhitespaces();
        if (!isAtIdentifier()) {
            context.error(reader.current(), "Expected an identifier.");
            context.skipErrors();
            return parseExpression();
        }

        Position declaration = reader.current();
        String variableName = readIdentifier();
        skipWhitespaces();

        Node variableValue = new Constant(declaration, null);
        if (reader.current().is('=')) {
            consumeExpectedCharacter('=');
            skipWhitespaces();
            variableValue = parseExpression();
        }

        if (context.getVariableScoper().resolve(variableName).isPresent()) {
            context.warning(declaration, "This declaration of '%s' shadows another variable.", variableName);
        }

        VariableScoper.Variable variable = context.getVariableScoper()
                                                  .defineVariable(declaration,
                                                                  variableName,
                                                                  variableValue.getType(),
                                                                  variableValue.getGenericType());
        return new Assignment(declaration, variable, variableValue);
    }

    /**
     * Parses an expression while updating the whitespace mode.
     *
     * @param skipWhitespaces determines if whitespaces will be skipped during parsing or not
     * @return the parsed expression
     */
    protected Node parseExpression(boolean skipWhitespaces) {
        boolean oldSkipWhitespaces = canSkipWhitespace;
        canSkipWhitespace = skipWhitespaces;
        try {
            if (!skipWhitespaces && reader.current().is('(')) {
                return atom();
            }

            return parseExpression();
        } finally {
            skipWhitespaces();
            canSkipWhitespace = oldSkipWhitespaces;
        }
    }

    /**
     * Parses an expression.
     *
     * @return the parsed expression
     */
    private Node parseExpression() {
        if (canSkipWhitespace) {
            return parseTernaryOperation(disjunction());
        } else {
            return chain();
        }
    }

    /**
     * Parses a ternary operation (if the input starts with '?'.
     * <p>
     * A ternary operation is <pre>CONDITION ? EXPRESSION_IF_TRUE : EXPRESSION_IF_FALSE</pre> or in short <pre>CONDITION
     * ? EXPRESSION_IF_TRUE</pre> which uses <tt>null</tt> when the condition is false.
     *
     * @param baseNode the condition which has already been parsed
     * @return the baseExpression if no '?' was found or an {@link TernaryOperation}
     */
    private Node parseTernaryOperation(Node baseNode) {
        skipWhitespaces();
        if (!reader.current().is('?')) {
            return baseNode;
        }

        assertType(baseNode.getPosition(), baseNode, boolean.class);
        reader.consume();
        Node whenTrue = parseExpression();
        Node whenFalse = new Constant(reader.current(), null);
        skipWhitespaces();
        if (reader.current().is(':')) {
            reader.consume();
            skipWhitespaces();
            whenFalse = parseExpression();
        }

        if (!CompilationContext.isAssignableTo(whenTrue.getType(), whenFalse.getType())
            && !CompilationContext.isAssignableTo(whenFalse.getType(), whenTrue.getType())) {
            context.error(baseNode.getPosition(),
                          "Both arms of a tenary expression need to be of matching types. Found: %s and %s",
                          whenTrue.getType(),
                          whenFalse.getType());
        }

        return new TernaryOperation(baseNode, whenTrue, whenFalse);
    }

    /**
     * Parses a {@link #conjunction()} and supports arbitrary many disjunctions ('||') or <b>the noodle operator</b>,
     * which is '|' and uses the first non null, non empty string value in the list.
     *
     * @return an expression which consists of zero to many disjunctions or noodle operations
     */
    private Node disjunction() {
        Node result = conjunction();

        while (!reader.current().isEndOfInput()) {
            skipWhitespaces();
            if (reader.current().is('|') && reader.next().is('|')) {
                Position pos = reader.current();
                reader.consume(2);
                Node right = conjunction();
                assertType(pos, result, boolean.class);
                assertType(pos, right, boolean.class);
                result = new Disjunction(pos, result, right);
            } else {
                break;
            }
        }

        return result;
    }

    /**
     * Parses a {@link #relationalExpression()} and supports arbitrary many conjunctions ('&&').
     *
     * @return an expression which consists of zero to many conjunctions
     */
    private Node conjunction() {
        Node result = relationalExpression();

        while (!reader.current().isEndOfInput()) {
            skipWhitespaces();
            if (reader.current().is('&') && reader.next().is('&')) {
                Position pos = reader.current();
                reader.consume(2);
                Node right = relationalExpression();
                assertType(pos, result, boolean.class);
                assertType(pos, right, boolean.class);
                result = new Conjunction(pos, result, right);
            } else {
                break;
            }
        }

        return result;
    }

    private void assertType(Position pos, Node node, Class<?> expectedType) {
        if (!CompilationContext.isAssignableTo(node.getType(), expectedType)) {
            context.error(pos, "Expected an expression of type %s but got %s", expectedType, node.getType());
            context.skipErrors();
        }
    }

    /**
     * Parses a {@link #term()} followed by an optional relational operation ('<', '<=', ... , '!=', '==').
     *
     * @return an expression which is either a term or a relational operation
     */
    private Node relationalExpression() {
        Node left = term();
        skipWhitespaces();
        if (reader.current().is('<') && !reader.next().is('/')) {
            return parseLessThan(left);
        }

        if (reader.current().is('>')) {
            return parseGreaterThan(left);
        }

        if (reader.current().is('!') && reader.next().is('=')) {
            return parseEquals(left, OpCode.OP_NE);
        }

        if (reader.current().is('=') && reader.next().is('=')) {
            return parseEquals(left, OpCode.OP_EQ);
        }

        return left;
    }

    private Node parseEquals(Node left, OpCode op) {
        Position position = reader.current();
        reader.consume(2);
        Node right = term();

        return new BinaryOperation(position, op, left, right, boolean.class);
    }

    private Node parseGreaterThan(Node left) {
        Position position = reader.current();
        reader.consume();

        if (reader.current().is('=')) {
            reader.consume();
            Node right = term();
            assertType(position, left, Comparable.class);
            assertType(position, right, Comparable.class);
            return new BinaryOperation(position, OpCode.OP_GE, left, right, boolean.class);
        } else {
            Node right = term();
            assertType(position, left, Comparable.class);
            assertType(position, right, Comparable.class);
            return new BinaryOperation(position, OpCode.OP_GT, left, right, boolean.class);
        }
    }

    private Node parseLessThan(Node left) {
        Position position = reader.current();
        reader.consume();
        if (reader.current().is('=')) {
            reader.consume();
            Node right = term();

            assertType(position, left, Comparable.class);
            assertType(position, right, Comparable.class);
            return new BinaryOperation(position, OpCode.OP_LE, left, right, boolean.class);
        } else {
            Node right = term();
            assertType(position, left, Comparable.class);
            assertType(position, right, Comparable.class);
            return new BinaryOperation(position, OpCode.OP_LT, left, right, boolean.class);
        }
    }

    /**
     * Parses a {@link #product()} followed by arbitrary many terms (+ X or -Y).
     * <p>
     * A special case are Strings, which are also concatenated by the '+' operator).
     *
     * @return an expression which is either a product or a term
     */
    private Node term() {
        Node result = product();

        while (!reader.current().isEndOfInput()) {
            skipWhitespaces();
            if (reader.current().is('+')) {
                Char operator = reader.consume();
                Node right = product();
                if (CompilationContext.isAssignableTo(result.getType(), Number.class)
                    && CompilationContext.isAssignableTo(right.getType(), Number.class)) {
                    result = new BinaryOperation(operator, OpCode.OP_ADD, result, right);
                } else if (result.getType() == String.class || right.getType() == String.class) {
                    result = new BinaryOperation(operator, OpCode.OP_CONCAT, result, right, String.class);
                } else {
                    context.error(operator,
                                  "Both operands of '+' must be either numeric or one of both must be a String."
                                  + " Types are: %s, %s",
                                  result.getType(),
                                  right.getType());
                }
            } else if (reader.current().is('-')) {
                Position position = reader.current();
                reader.consume();
                Node right = product();
                assertType(position, result, Number.class);
                assertType(position, right, Number.class);
                result = new BinaryOperation(position, OpCode.OP_SUB, result, right);
            } else {
                break;
            }
        }

        return result;
    }

    /**
     * Parses a {@link #chain()} followed by arbitrary many products (* X,  / Y, % Z).
     *
     * @return an expression which is either a method call chain or a product
     */
    private Node product() {
        Node result = chain();

        while (!reader.current().isEndOfInput()) {
            skipWhitespaces();
            if (reader.current().is('*')) {
                Position position = reader.current();
                reader.consume();
                Node rightNode = chain();
                assertType(position, result, Number.class);
                assertType(position, rightNode, Number.class);
                result = new BinaryOperation(position, OpCode.OP_MUL, result, rightNode);
            } else if (reader.current().is('/')) {
                Position position = reader.current();
                reader.consume();
                Node rightNode = chain();
                assertType(position, result, Number.class);
                assertType(position, rightNode, Number.class);
                result = new BinaryOperation(position, OpCode.OP_DIV, result, rightNode);
            } else if (reader.current().is('%')) {
                Position position = reader.current();
                reader.consume();
                Node rightNode = chain();
                assertType(position, result, Number.class);
                assertType(position, rightNode, Number.class);
                result = new BinaryOperation(position, OpCode.OP_MOD, result, rightNode);
                assertType(position, result, int.class);
                assertType(position, rightNode, int.class);
            } else {
                break;
            }
        }

        return result;
    }

    /**
     * Parses an {@link #atom()} which can be followed by one or more method calls.
     *
     * @return an expression which is either an atom or a method call chain
     */
    private Node chain() {
        Node root = atom();
        while (!reader.current().isEndOfInput()) {
            skipWhitespaces();
            if (reader.current().is('.')) {
                reader.consume();
                root = chainContinuation(root);
            } else {
                break;
            }
        }

        return root;
    }

    /**
     * Parses a single method call.
     *
     * @param self the expression on which the method is invoked
     * @return the method call as expression
     */
    private Node chainContinuation(Node self) {
        skipWhitespaces();
        if (!isAtIdentifier()) {
            context.error(reader.current(), "Expected a method name.");
            context.skipErrors();
        }
        Char position = reader.current();
        String nextIdentifier = readIdentifier();

        if (nextIdentifier.equals(KEYWORD_CLASS) && self instanceof RawClassLiteral) {
            return new Constant(position, self.getConstantValue());
        }

        skipWhitespaces();

        if (reader.current().is('(')) {
            return call(self, position, nextIdentifier);
        }

        if (self instanceof RawClassLiteral) {
            Node innerClass = tryResolveStaticClassMember(position, (Class<?>) self.getConstantValue(), nextIdentifier);
            if (innerClass != null) {
                return innerClass;
            }
        }

        Field targetField = Reflection.getAllFields(self.getType())
                                      .stream()
                                      .filter(field -> !Modifier.isStatic(field.getModifiers()))
                                      .filter(field -> field.getName().equals(nextIdentifier))
                                      .findAny()
                                      .orElse(null);
        if (targetField != null) {
            if (!context.isSandboxEnabled() && !Modifier.isPublic(targetField.getModifiers())) {
                context.error(position,
                              "The field '%s' of '%s' is not public accessible.",
                              targetField.getName(),
                              targetField.getDeclaringClass().getName());
            }

            PushField pushField = new PushField(position, targetField);
            pushField.setSelfExpression(self);
            return pushField;
        }

        context.error(position, "Unexpected identifier. '%s' is neither a method nor a static member.", nextIdentifier);
        context.skipErrors();
        return new Constant(position, null);
    }

    private Node tryResolveStaticClassMember(Char position, Class<?> parentClass, String nextIdentifier) {
        for (Class<?> inner : parentClass.getClasses()) {
            if (inner.getSimpleName().equals(nextIdentifier)) {
                return new RawClassLiteral(position, inner);
            }
        }

        Field staticField = Reflection.getAllFields(parentClass)
                                      .stream()
                                      .filter(field -> Modifier.isStatic(field.getModifiers()))
                                      .filter(field -> field.getName().equals(nextIdentifier))
                                      .findAny()
                                      .orElse(null);
        if (staticField != null) {
            if (!context.isSandboxEnabled() && !Modifier.isPublic(staticField.getModifiers())) {
                context.error(position,
                              "The field '%s' of '%s' is not public accessible.",
                              staticField.getName(),
                              staticField.getDeclaringClass().getName());
            }

            return new PushField(position, staticField);
        }

        if (parentClass.isEnum()) {
            for (Object enumValue : parentClass.getEnumConstants()) {
                if (Strings.areEqual(((Enum<?>) enumValue).name(), nextIdentifier)) {
                    return new Constant(position, enumValue);
                }
            }
        }

        return null;
    }

    private Node call(Node self, Char position, String methodName) {
        consumeExpectedCharacter('(');
        List<Node> parameters = parseParameterList();
        consumeExpectedCharacter(')');
        Node specialNode = handleSpecialMethods(self, methodName, parameters);
        if (specialNode != null) {
            return specialNode;
        }

        MethodCall call = new MethodCall(position, self, methodName);
        call.setParameters(parameters);
        call.tryBindToMethod(context);

        return call;
    }

    /**
     * Handles special methods like <tt>.is</tt> and <tt>.as</tt>
     * <p>
     * To make the syntax a bit more pleasing we do casts as ".as" operation instead of double brackets. We also use
     * <tt>.is</tt> instead of "instanceof" as it can be written without whitespaces...
     *
     * @param self       the expression to invoke a method on
     * @param methodName the name of the method to invoke
     * @param parameters the parameters for the method
     * @return either a {@link NativeCast} or an {@link InstanceOfCheck} or <tt>null</tt> if the call isn't a cast
     */
    @Nullable
    private Node handleSpecialMethods(Node self, String methodName, List<Node> parameters) {
        if (KEYWORD_METHOD_AS.equals(methodName)
            && parameters.size() == 1
            && parameters.get(0).isConstant()
            && (Class.class.isAssignableFrom(parameters.get(0).getType()))) {
            Class<?> type = (Class<?>) parameters.get(0).getConstantValue();
            if (!Transformable.class.isAssignableFrom(self.getType())) {
                return new NativeCast(self.getPosition(), self, type);
            }
        }

        if (KEYWORD_METHOD_IS.equals(methodName)
            && parameters.size() == 1
            && parameters.get(0).isConstant()
            && (Class.class.isAssignableFrom(parameters.get(0).getType()))) {
            Class<?> type = (Class<?>) parameters.get(0).getConstantValue();
            if (!Transformable.class.isAssignableFrom(self.getType())) {
                return new InstanceOfCheck(self.getPosition(), self, type);
            }
        }

        return null;
    }

    /**
     * Parses the list of parameters for a {@link #chainContinuation(Node)} or {@link #macroCall(Char, String)}.
     *
     * @return the list of parameter expressions for the method call
     */
    private List<Node> parseParameterList() {
        if (reader.current().is(')')) {
            return Collections.emptyList();
        }

        List<Node> parameters = new ArrayList<>();
        while (!reader.current().isEndOfInput()) {
            parameters.add(parseExpression(true));
            if (!reader.current().is(',')) {
                break;
            }
            consumeExpectedCharacter(',');
            skipWhitespaces();
        }
        return parameters;
    }

    /**
     * Parses an atom.
     * <p>
     * This is either:
     * <ul>
     * <li>an expression in brackets</li>
     * <li>a variable</li>
     * <li>a static call</li>
     * <li>a literal</li>
     * </ul>
     *
     * @return the atom as expression
     */
    private Node atom() {
        skipWhitespaces();
        if (reader.current().is('!')) {
            Position position = reader.consume();
            Node target = chain();
            if (!Boolean.class.equals(CompilationContext.autoboxClass(target.getType()))) {
                context.error(position, "Expected a boolean expression here!");
            }
            return new UnaryOperation(position, OpCode.OP_NOT, target);
        }
        if (reader.current().is('(')) {
            reader.consume();
            Node result = parseExpression(true);
            consumeExpectedCharacter(')');

            return result;
        }

        if (isAtIdentifier()) {
            Char position = reader.current();
            String identifier = readIdentifier();
            if (reader.current().is('.')) {
                Node result = tryVariable(position, identifier);
                if (result != null) {
                    return result;
                }
                return tryClassLiteral(position, identifier);
            }

            skipWhitespaces();

            if (reader.current().is('(')) {
                return macroCall(position, identifier);
            }
            if (reader.current().is('=') && !reader.next().is('=')) {
                return assignmentTo(position, identifier);
            }

            return variable(position, identifier);
        }

        return literal();
    }

    private Node assignmentTo(Char position, String identifier) {
        consumeExpectedCharacter('=');
        skipWhitespaces();
        Node variableValue = parseExpression();
        VariableScoper.Variable var = context.getVariableScoper().resolve(identifier).orElse(null);
        if (var == null) {
            context.error(position, "Unknown variable '%s'. Add 'let' to define a variable.", identifier);
            return variableValue;
        }

        return new Assignment(position, var, variableValue);
    }

    private Node tryClassLiteral(Position position, String identifier) {
        StringBuilder className = new StringBuilder(identifier);
        while (true) {
            Class<?> clazz = context.tryResolveClass(className.toString()).orElse(null);
            if (clazz != null) {
                return new RawClassLiteral(position, clazz);
            }

            if (!reader.current().is('.')) {
                return reportUnknownVariableOrClass(position, identifier, className.toString());
            }
            className.append(reader.consume().getStringValue());

            if (!isAtIdentifier()) {
                return reportUnknownVariableOrClass(position, identifier, className.toString());
            }
            className.append(readIdentifier());
        }
    }

    private Node reportUnknownVariableOrClass(Position position, String identifier, String className) {
        String localClassName = Strings.splitAtLast(className, ".").getSecond();
        if (Strings.isEmpty(localClassName) || !Character.isUpperCase(localClassName.charAt(0))) {
            context.error(position, "Unknown variable %s", identifier);
        } else {
            context.error(position, "Unknown class %s", className);
        }
        context.skipErrors();

        return new Constant(position, null);
    }

    /**
     * Parses a static call to a {@link sirius.pasta.noodle.macros.Macro}.
     *
     * @param position   the position of the invokation
     * @param methodName the name of the macro to call
     * @return the parsed {@link MacroCall}
     */
    private Node macroCall(Char position, String methodName) {
        if ("const".equals(methodName)) {
            return handleConstExpr(position);
        }
        consumeExpectedCharacter('(');
        MacroCall call = new MacroCall(position, methodName);
        call.setParameters(parseParameterList());
        consumeExpectedCharacter(')');
        call.tryBind(context);

        return call;
    }

    private Constant handleConstExpr(Char position) {
        consumeExpectedCharacter('(');
        if (reader.current().is(')')) {
            context.error(reader.current(), "Expected an inner expression for const");
            context.skipErrors();
            return new Constant(reader.consume(), null);
        } else {
            Node constExpr = parseExpression(true);
            consumeExpectedCharacter(')');

            try {
                Assembler assembler = new Assembler();
                constExpr.emit(assembler);
                InterpreterCall expr =
                        assembler.build(constExpr.getType(), constExpr.getGenericType(), context.getSourceCodeInfo());
                Object value = expr.call(null);
                return new Constant(position, value);
            } catch (HandledException e) {
                context.error(position, "Failed to compile and evaluate const expression: %s", e.getMessage());
                return new Constant(reader.consume(), null);
            } catch (Exception e) {
                context.error(position,
                              "Failed to compile and evaluate const expression: %s in: %s - %s",
                              constExpr.toString(),
                              context.getSourceCodeInfo().getName(),
                              Exceptions.handle(Pasta.LOG, e).getMessage());
                return new Constant(reader.consume(), null);
            }
        }
    }

    /**
     * Parses a variable reference.
     * <p>
     * This will be either a well known keyword (true, false, null) or a local or global variable.
     *
     * @param position     the position where the identifier started
     * @param variableName the parsed variable name
     * @return either a representation of a constant value or a read operation on the respective local or global
     * variable
     */
    @Nullable
    private Node tryVariable(Char position, String variableName) {
        if (KEYWORD_TRUE.equalsIgnoreCase(variableName)) {
            return new Constant(position, true);
        }

        if (KEYWORD_FALSE.equalsIgnoreCase(variableName)) {
            return new Constant(position, false);
        }

        if (KEYWORD_NULL.equalsIgnoreCase(variableName)) {
            return new Constant(position, null);
        }

        Optional<VariableScoper.Variable> variable = context.getVariableScoper().resolve(variableName);
        if (variable.isPresent()) {
            return new PushTemporary(position, variable.get());
        }

        for (LegacyGlobalsHandler legacyGlobalsHandler : legacyGlobalsHandlers) {
            Tuple<String, Node> replacement = legacyGlobalsHandler.replaceLegacyVariable(context, variableName);
            if (replacement != null) {
                context.warning(position,
                                "Replacing legacy variable '%s' with '%s'!",
                                variableName,
                                replacement.getFirst());
                return replacement.getSecond();
            }
        }

        return null;
    }

    /**
     * Parses a variable reference.
     * <p>
     * This will be either a well known keyword (true, false, null) or a local or global variable.
     *
     * @param position     the position where the identifier started
     * @param variableName the parsed variable name
     * @return either a representation of a constant value or a read operation on the respective local or global
     * variable
     */
    private Node variable(Char position, String variableName) {
        Node result = tryVariable(position, variableName);
        if (result == null) {
            context.error(position, "Unknown variable %s", variableName);
            return new Constant(position, null);
        } else {
            return result;
        }
    }

    /**
     * Parses a literal string or number.
     *
     * @return the parsed string ror number
     */
    private Node literal() {
        if (reader.current().is('\'')) {
            return string('\'');
        }
        if (reader.current().is('"')) {
            return string('"');
        }
        if (reader.current().isDigit() || reader.current().is('-') && reader.next().isDigit()) {
            return number();
        }

        context.error(reader.current(), "Unexpected Token: %s", reader.current().getValue());
        context.skipErrors();

        return new Constant(reader.consume(), null);
    }

    /**
     * Parses a literal number.
     *
     * @return the parsed number
     */
    private Node number() {
        Position position = reader.current();
        boolean decimal = false;
        StringBuilder numericLiteral = new StringBuilder(reader.consume().getStringValue());
        while (reader.current().isDigit()) {
            numericLiteral.append(reader.consume().getValue());
        }

        if (reader.current().is('.') && reader.next().isDigit()) {
            numericLiteral.append(reader.consume().getValue());
            while (reader.current().isDigit()) {
                numericLiteral.append(reader.consume().getValue());
            }
            decimal = true;
        }

        if (decimal) {
            return new Constant(position, Double.parseDouble(numericLiteral.toString()));
        }

        long longValue = Long.parseLong(numericLiteral.toString());
        if (longValue <= Integer.MAX_VALUE && longValue >= Integer.MIN_VALUE) {
            return new Constant(position, (int) longValue);
        } else {
            return new Constant(position, longValue);
        }
    }

    /**
     * Parses a string.
     *
     * @param stopChar the string delimiter which was used to start the string.
     * @return the parsed string
     */
    private Node string(char stopChar) {
        Position position = reader.current();
        reader.consume();
        StringBuilder stringLiteral = new StringBuilder();
        while (!reader.current().isEndOfInput() && !reader.current().is(stopChar) && !reader.current().isNewLine()) {
            if (reader.current().is('\\')) {
                reader.consume();
                if (reader.current().is('n')) {
                    reader.consume();
                    stringLiteral.append("\n");
                } else if (reader.current().is('t')) {
                    reader.consume();
                    stringLiteral.append("\t");
                } else {
                    stringLiteral.append(reader.consume().getValue());
                }
            } else {
                stringLiteral.append(reader.consume().getValue());
            }
        }
        consumeExpectedCharacter(stopChar);

        return new Constant(position, stringLiteral.toString());
    }

    /**
     * Fully reads an identifier name.
     *
     * @return the parsed identifier
     */
    private String readIdentifier() {
        StringBuilder identifier = new StringBuilder();
        while (reader.current().isLetter() || reader.current().is('_') || reader.current().isDigit()) {
            identifier.append(reader.consume().getValue());
        }

        return identifier.toString();
    }

    /**
     * Determines if the input is currently at the start of an identifier.
     *
     * @return <tt>true</tt> if the current character can be the start of an identifier, <tt>false</tt> otherwise
     */
    private boolean isAtIdentifier() {
        return reader.current().isLetter() || reader.current().is('_');
    }

    private boolean isAtKeyword(String keyword) {
        for (int i = 0; i < keyword.length(); i++) {
            if (reader.next(i).getValue() != keyword.charAt(i)) {
                return false;
            }
        }

        Char charAfterKeyword = reader.next(keyword.length());
        return !charAfterKeyword.isLetter() && !charAfterKeyword.isDigit() && !charAfterKeyword.is('_');
    }

    @Override
    public int skipWhitespaces() {
        if (!canSkipWhitespace) {
            return 0;
        }

        return super.skipWhitespaces();
    }
}
