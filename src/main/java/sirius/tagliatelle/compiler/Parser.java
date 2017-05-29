/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import parsii.tokenizer.Char;
import parsii.tokenizer.LookaheadReader;
import sirius.kernel.commons.Tuple;
import sirius.tagliatelle.expression.ConcatExpression;
import sirius.tagliatelle.expression.ConstantBoolean;
import sirius.tagliatelle.expression.ConstantInt;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.IntOperation;
import sirius.tagliatelle.expression.MacroCall;
import sirius.tagliatelle.expression.MethodCall;
import sirius.tagliatelle.expression.NoodleOperation;
import sirius.tagliatelle.expression.OperationAnd;
import sirius.tagliatelle.expression.OperationEquals;
import sirius.tagliatelle.expression.OperationOr;
import sirius.tagliatelle.expression.Operator;
import sirius.tagliatelle.expression.ReadGlobal;
import sirius.tagliatelle.expression.ReadLocal;
import sirius.tagliatelle.expression.RelationalIntOperation;
import sirius.tagliatelle.expression.TenaryOperation;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Parses {@link Expression expressions} used by tagliatelle.
 * <p>
 * The parser is invoked by the {@link Compiler} once an expression was detected. It is implemented as a simple
 * recursive descending parser.
 */
class Parser extends InputProcessor {

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

    /**
     * Creates a new parser for the given input and context.
     *
     * @param reader  the input to process
     * @param context the context to use
     */
    protected Parser(LookaheadReader reader, CompilationContext context) {
        super(reader, context);
    }

    /**
     * Parses and optimizes an expression from the given input.
     *
     * @param skipWhitespaces determines if initially whitespaces can be skipped or not
     * @return the parsed expression
     */
    protected Expression parse(boolean skipWhitespaces) {
        return parseExpression(skipWhitespaces).reduce();
    }

    /**
     * Parses an expression while updating the whitespace mode.
     *
     * @param skipWhitespaces determines if whitespaces will be skipped during parsing or not
     * @return the parsed expression
     */
    private Expression parseExpression(boolean skipWhitespaces) {
        boolean oldSkipWhitespaces = canSkipWhitespace;
        canSkipWhitespace = skipWhitespaces;
        try {
            if (!skipWhitespaces && reader.current().is('(')) {
                return atom();
            }

            return parseExpression();
        } finally {
            skipUnexpectedWhitespace();
            canSkipWhitespace = oldSkipWhitespaces;
        }
    }

    /**
     * Parses an expression.
     *
     * @return the parsed expression
     */
    private Expression parseExpression() {
        Expression result = disjunction();
        if (canSkipWhitespace) {
            result = parseTenaryOperation(result);
        }
        return result;
    }

    /**
     * Parses a tenary operation (if the input starts with '?'.
     * <p>
     * A tenary operation is <pre>CONDITION ? EXPRESSION_IF_TRUE : EXPRESSION_IF_FALSE</pre> or in short <pre>CONDITION
     * ? EXPRESSION_IF_TRUE</pre> which uses <tt>null</tt> when the condition is false.
     *
     * @param baseExpression the condition which has already been parsed
     * @return the baseExpression if no '?' was found or an {@link TenaryOperation}
     */
    private Expression parseTenaryOperation(Expression baseExpression) {
        skipExpectedWhitespace();
        if (!reader.current().is('?')) {
            return baseExpression;
        }

        reader.consume();
        Expression whenTrue = parseExpression();
        Expression whenFalse = ConstantNull.NULL;
        skipExpectedWhitespace();
        if (reader.current().is(':')) {
            reader.consume();
            skipExpectedWhitespace();
            whenFalse = parseExpression();
        }

        return new TenaryOperation(baseExpression, whenTrue, whenFalse);
    }

    /**
     * Parses a {@link #conjunction()} and supports arbitrary many disjunctions ('||') or <b>the nooble operator</b>,
     * which is '|' and uses the first non null, non empty string value in the list.
     *
     * @return an expression which consists of zero to many disjunctions or noodle operations
     */
    private Expression disjunction() {
        Expression result = conjunction();

        while (!reader.current().isEndOfInput()) {
            skipExpectedWhitespace();
            if (reader.current().is('|')) {
                if (reader.next().is('|')) {
                    reader.consume(2);
                    result = new OperationOr(result, conjunction());
                } else {
                    reader.consume();
                    result = new NoodleOperation(result, conjunction());
                }
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
    private Expression conjunction() {
        Expression result = relationalExpression();

        while (!reader.current().isEndOfInput()) {
            skipExpectedWhitespace();
            if (reader.current().is('&') && reader.next().is('&')) {
                reader.consume(2);
                result = new OperationAnd(result, relationalExpression());
            } else {
                break;
            }
        }

        return result;
    }

    /**
     * Parses a {@link #term()} followed by an optional relational operation ('<', '<=', ... , '!=', '==').
     *
     * @return an expression which is either a term or a relational operation
     */
    private Expression relationalExpression() {
        Expression left = term();
        skipExpectedWhitespace();
        if (reader.current().is('<') && !reader.next().is('/')) {
            return parseLessThan(left);
        }

        if (reader.current().is('>')) {
            return parseGreaterThan(left);
        }

        if (reader.current().is('!') && reader.next().is('=')) {
            return parseEquals(left, Operator.NE);
        }

        if (reader.current().is('=') && reader.next().is('=')) {
            return parseEquals(left, Operator.EQ);
        }

        return left;
    }

    private Expression parseEquals(Expression left, Operator op) {
        reader.consume(2);
        Expression right = term();
        if (left.getType() == int.class && right.getType() == int.class) {
            return new RelationalIntOperation(op, left, right);
        } else {
            return new OperationEquals(left, right, op == Operator.NE);
        }
    }

    private Expression parseGreaterThan(Expression left) {
        reader.consume();
        if (reader.current().is('=')) {
            reader.consume();
            return new RelationalIntOperation(Operator.GT_EQ, left, term());
        } else {
            return new RelationalIntOperation(Operator.GT, left, term());
        }
    }

    private Expression parseLessThan(Expression left) {
        reader.consume();
        if (reader.current().is('=')) {
            reader.consume();
            return new RelationalIntOperation(Operator.LT_EQ, left, term());
        } else {
            return new RelationalIntOperation(Operator.LT, left, term());
        }
    }

    /**
     * Parses a {@link #product()} followed by arbitrary many terms (+ X or -Y).
     * <p>
     * A special case are Strings, which are also concatenated by the '+' operator).
     *
     * @return an expression which is either a product or a term
     */
    private Expression term() {
        Expression result = product();

        while (!reader.current().isEndOfInput()) {
            skipExpectedWhitespace();
            if (reader.current().is('+')) {
                Char operator = reader.consume();
                Expression right = product();
                if (result.getType() == int.class && right.getType() == int.class) {
                    result = new IntOperation(Operator.ADD, result, right);
                } else if (result instanceof ConcatExpression) {
                    ((ConcatExpression) result).add(right);
                } else if (result.getType() == String.class || right.getType() == String.class) {
                    result = new ConcatExpression(result, right);
                } else {
                    context.error(operator,
                                  "Both operands of '+' must be either an int or one of both must be a String."
                                  + " Types are: %s, %s",
                                  result.getType(),
                                  right.getType());
                }
            } else if (reader.current().is('-')) {
                reader.consume();
                result = new IntOperation(Operator.SUBTRACT, result, product());
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
    private Expression product() {
        Expression result = chain();

        while (!reader.current().isEndOfInput()) {
            skipExpectedWhitespace();
            if (reader.current().is('*')) {
                reader.consume();
                result = new IntOperation(Operator.MULTIPLY, result, chain());
            } else if (reader.current().is('/')) {
                reader.consume();
                result = new IntOperation(Operator.DIVIDE, result, chain());
            } else if (reader.current().is('%')) {
                reader.consume();
                result = new IntOperation(Operator.MODULO, result, chain());
            } else {
                break;
            }
        }

        return result;
    }

    /**
     * Parses an {@link #atom()} which can be folloed by one or more method calls.
     *
     * @return an expression which is either an atom or a method call chain
     */
    private Expression chain() {
        Expression root = atom();
        while (!reader.current().isEndOfInput()) {
            skipUnexpectedWhitespace();
            if (reader.current().is('.')) {
                reader.consume();
                root = call(root);
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
    private Expression call(Expression self) {
        skipUnexpectedWhitespace();
        if (!isAtIdentifier()) {
            context.error(reader.current(), "Expected a method name.");
        }
        Char position = reader.current();
        String methodName = readIdentifier();
        skipUnexpectedWhitespace();
        consumeExpectedCharacter('(');
        MethodCall call = new MethodCall(self);
        call.setParameters(parseParameterList());
        call.bindToMethod(position, context, methodName);
        consumeExpectedCharacter(')');

        return call;
    }

    /**
     * Parses the list of parameters for a {@link #call(Expression)} or {@link #staticCall(Char, String)}.
     *
     * @return the list of parameter expressions for the method call
     */
    private List<Expression> parseParameterList() {
        if (reader.current().is(')')) {
            return Collections.emptyList();
        }

        List<Expression> parameters = new ArrayList<>();
        while (!reader.current().isEndOfInput()) {
            parameters.add(parseExpression(true));
            if (!reader.current().is(',')) {
                break;
            }
            consumeExpectedCharacter(',');
            skipExpectedWhitespace();
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
    private Expression atom() {
        skipUnexpectedWhitespace();
        if (reader.current().is('(')) {
            reader.consume();
            Expression result = parseExpression(true);
            consumeExpectedCharacter(')');
            return result;
        }

        if (isAtIdentifier()) {
            Char pos = reader.current();
            String id = readIdentifier();
            skipUnexpectedWhitespace();

            if (reader.current().is('(')) {
                return staticCall(pos, id);
            }

            return variable(pos, id);
        }

        return literal();
    }

    /**
     * Parses a static call to a {@link sirius.tagliatelle.macros.Macro}.
     *
     * @param pos        the position of the invokation
     * @param methodName the name of the macro to call
     * @return the parsed {@link MacroCall}
     */
    private Expression staticCall(Char pos, String methodName) {
        skipUnexpectedWhitespace();
        consumeExpectedCharacter('(');
        MacroCall call = new MacroCall();
        call.setParameters(parseParameterList());
        call.bindToMethod(context, methodName);
        consumeExpectedCharacter(')');

        return call;
    }

    /**
     * Parses a variable reference.
     * <p>
     * This will be either a well known keyword (true, false, null) or a local or global variable.
     *
     * @param pos          the position where the identifier started
     * @param variableName the parsed variable name
     * @return either a representation of a constant value or a read operation on the respective local or global
     * variable
     */
    private Expression variable(Char pos, String variableName) {
        if ("true".equalsIgnoreCase(variableName)) {
            return ConstantBoolean.TRUE;
        }

        if ("false".equalsIgnoreCase(variableName)) {
            return ConstantBoolean.FALSE;
        }

        if ("null".equalsIgnoreCase(variableName)) {
            return ConstantNull.NULL;
        }

        Optional<Tuple<Class<?>, Integer>> variable = context.findLocal(variableName);
        if (variable.isPresent()) {
            return new ReadLocal(variable.get().getFirst(), variable.get().getSecond());
        }

        variable = context.findGlobal(variableName);
        if (variable.isPresent()) {
            return new ReadGlobal(variable.get().getFirst(), variable.get().getSecond());
        }

        context.error(pos, "Unknown variable %s", variableName);
        return ConstantNull.NULL;
    }

    /**
     * Parses a literal string or number.
     *
     * @return the parsed string ror number
     */
    private Expression literal() {
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
        return ConstantNull.NULL;
    }

    /**
     * Parses a literal number.
     *
     * @return the parsed number
     */
    private Expression number() {
        StringBuilder sb = new StringBuilder(reader.consume().getStringValue());
        while (reader.current().isDigit()) {
            sb.append(reader.consume().getValue());
        }

        String value = sb.toString();

        if ("0".equals(value)) {
            return ConstantInt.ZERO;
        }

        if ("1".equals(value)) {
            return ConstantInt.ONE;
        }

        if ("-1".equals(value)) {
            return ConstantInt.MINUS_ONE;
        }

        return new ConstantInt(Integer.parseInt(value));
    }

    /**
     * Parses a string.
     *
     * @param stopChar the string delimiter which was used to start the string.
     * @return the parsed string
     */
    private Expression string(char stopChar) {
        reader.consume();
        StringBuilder sb = new StringBuilder();
        while (!reader.current().isEndOfInput() && !reader.current().is(stopChar)) {
            if (reader.current().is('\\')) {
                reader.consume();
                if (reader.current().is('n')) {
                    reader.consume();
                    sb.append("\n");
                } else if (reader.current().is('t')) {
                    reader.consume();
                    sb.append("\t");
                } else {
                    sb.append(reader.consume().getValue());
                }
            } else {
                sb.append(reader.consume().getValue());
            }
        }
        consumeExpectedCharacter(stopChar);

        return new ConstantString(sb.toString());
    }

    /**
     * Fully reads an identifier name.
     *
     * @return the parsed identifier
     */
    private String readIdentifier() {
        StringBuilder sb = new StringBuilder();
        while (reader.current().isLetter() || reader.current().is('_') || reader.current().isDigit()) {
            sb.append(reader.consume().getValue());
        }

        return sb.toString();
    }

    /**
     * Determines if the input is currently at the start of an identifier.
     *
     * @return <tt>true</tt> if the current character can be the start of an identifier, <tt>false</tt> otherwise
     */
    private boolean isAtIdentifier() {
        return reader.current().isLetter() || reader.current().is('_');
    }

    @Override
    protected void skipExpectedWhitespace() {
        if (!canSkipWhitespace) {
            return;
        }

        super.skipExpectedWhitespace();
    }

    @Override
    protected void skipUnexpectedWhitespace() {
        if (!canSkipWhitespace) {
            return;
        }
        super.skipUnexpectedWhitespace();
    }
}
