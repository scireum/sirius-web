/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import parsii.tokenizer.Char;
import parsii.tokenizer.LookaheadReader;
import sirius.kernel.commons.Tuple;
import sirius.web.templates.engine.expression.ConcatExpression;
import sirius.web.templates.engine.expression.ConstantBoolean;
import sirius.web.templates.engine.expression.ConstantInt;
import sirius.web.templates.engine.expression.ConstantNull;
import sirius.web.templates.engine.expression.ConstantString;
import sirius.web.templates.engine.expression.Expression;
import sirius.web.templates.engine.expression.IntOperation;
import sirius.web.templates.engine.expression.MethodCall;
import sirius.web.templates.engine.expression.OperationAnd;
import sirius.web.templates.engine.expression.OperationEquals;
import sirius.web.templates.engine.expression.OperationOr;
import sirius.web.templates.engine.expression.Operator;
import sirius.web.templates.engine.expression.ReadGlobal;
import sirius.web.templates.engine.expression.ReadLocal;
import sirius.web.templates.engine.expression.RelationalIntOperation;
import sirius.web.templates.engine.expression.StaticMethodCall;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Created by aha on 11.05.17.
 */
public class Parser extends InputProcessor {

    private boolean canSkipWhitespace = false;

    public Parser(LookaheadReader reader, CompilationContext context) {
        super(reader, context);
    }

    public Expression parse(boolean skipWhitespaces) {
        return parseExpression(skipWhitespaces);
    }

    private Expression parseExpression(boolean skipWhitespaces) {
        boolean oldSkipWhitespaces = canSkipWhitespace;
        canSkipWhitespace = skipWhitespaces;
        try {
            return disjunction();
        } finally {
            skipUnexpectedWhitespace();
            canSkipWhitespace = oldSkipWhitespaces;
        }
    }

    private Expression disjunction() {
        Expression result = conjunction();

        while (!reader.current().isEndOfInput()) {
            skipExpectedWhitespace();
            if (reader.current().is('|') && reader.next().is('|')) {
                reader.consume(2);
                result = new OperationOr(result, conjunction());
            } else {
                break;
            }
        }

        return result;
    }

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

    private Expression relationalExpression() {
        Expression left = term();
        skipExpectedWhitespace();
        if (reader.current().is('<')) {
            reader.consume();
            if (reader.current().is('=')) {
                reader.consume();
                return new RelationalIntOperation(Operator.LT_EQ, left, term());
            } else {
                return new RelationalIntOperation(Operator.LT, left, term());
            }
        }

        if (reader.current().is('>')) {
            reader.consume();
            if (reader.current().is('=')) {
                reader.consume();
                return new RelationalIntOperation(Operator.GT_EQ, left, term());
            } else {
                return new RelationalIntOperation(Operator.GT, left, term());
            }
        }

        if (reader.current().is('!') && reader.next().is('=')) {
            reader.consume(2);
            Expression right = term();
            if (left.getType() == int.class && right.getType() == int.class) {
                return new RelationalIntOperation(Operator.NE, left, right);
            } else {
                return new OperationEquals(left, right, true);
            }
        }

        if (reader.current().is('=') && reader.next().is('=')) {
            reader.consume(2);
            Expression right = term();
            if (left.getType() == int.class && right.getType() == int.class) {
                return new RelationalIntOperation(Operator.EQ, left, right);
            } else {
                return new OperationEquals(left, right, false);
            }
        }

        return left;
    }

    private Expression term() {
        Expression result = product();

        while (!reader.current().isEndOfInput()) {
            skipExpectedWhitespace();
            if (reader.current().is('+')) {
                Char operator = reader.consume();
                Expression right = product();
                if (result.getType() == int.class && right.getType() == int.class) {
                    result = new IntOperation(Operator.ADD, result,right);
                } else if (result instanceof ConcatExpression) {
                    ((ConcatExpression) result).add(right);
                } else if (result.getType() == String.class || right.getType() == String.class) {
                    result = new ConcatExpression(result, right);
                } else {
                    context.error(operator,
                                  "Both operands of '+' must be either an int or one of both must be a String. Types are: %s, %s",
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

    private Expression call(Expression self) {
        skipUnexpectedWhitespace();
        if (!isAtIdentifier()) {
            context.error(reader.current(), "Expected a method name.");
        }
        String methodName = readIdentifier();
        skipUnexpectedWhitespace();
        consumeExpectedCharacter('(');
        MethodCall call = new MethodCall(self);
        call.setParameters(parseParameterList());
        call.bindToMethod(methodName);
        consumeExpectedCharacter(')');

        return call;
    }

    private List<Expression> parseParameterList() {
        if (reader.current().is(')')) {
            return Collections.emptyList();
        }

        List<Expression> parameters = new ArrayList<>();
        while (!reader.current().isEndOfInput()) {
            parameters.add(parseExpression(true));
            if (reader.current().is(')')) {
                break;
            }
            consumeExpectedCharacter(',');
            skipExpectedWhitespace();
        }
        return parameters;
    }

    private Expression atom() {
        skipUnexpectedWhitespace();
        if (reader.current().is('(')) {
            reader.consume();
            Expression result = parseExpression(true);
            consumeExpectedCharacter(')');
            return result;
        }

        if (isAtIdentifier()) {
            String id = readIdentifier();
            skipUnexpectedWhitespace();

            if (reader.current().is('(')) {
                return staticCall(id);
            }

            return variable(id);
        }

        return literal();
    }

    private Expression staticCall(String methodName) {
        skipUnexpectedWhitespace();
        consumeExpectedCharacter('(');
        StaticMethodCall call = new StaticMethodCall();
        call.setParameters(parseParameterList());
        call.bindToMethod(context, methodName);
        consumeExpectedCharacter(')');

        return call;
    }

    private Expression variable(String variableName) {
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

        throw new IllegalArgumentException("UNKNOWN VARIABLE");
    }

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

    private String readIdentifier() {
        StringBuilder sb = new StringBuilder();
        while (reader.current().isLetter() || reader.current().is('_') || reader.current().isDigit()) {
            sb.append(reader.consume().getValue());
        }

        return sb.toString();
    }

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
