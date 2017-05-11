/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import parsii.tokenizer.LookaheadReader;
import sirius.kernel.commons.Tuple;
import sirius.web.templates.engine.expression.ConstantBoolean;
import sirius.web.templates.engine.expression.ConstantInt;
import sirius.web.templates.engine.expression.ConstantNull;
import sirius.web.templates.engine.expression.ConstantString;
import sirius.web.templates.engine.expression.Expression;
import sirius.web.templates.engine.expression.MethodCall;
import sirius.web.templates.engine.expression.ReadGlobal;
import sirius.web.templates.engine.expression.ReadLocal;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Created by aha on 11.05.17.
 */
public class Parser {

    private LookaheadReader reader;
    private CompilationContext context;
    private boolean canSkipWhitespace = false;

    public Parser(LookaheadReader reader, CompilationContext context) {
        this.reader = reader;
        this.context = context;
    }

    public Expression parse() {
        return parseExpression(false);
    }

    private Expression parseExpression(boolean skipWhitespaces) {
        boolean oldSkipWhitespaces = canSkipWhitespace;
        canSkipWhitespace = skipWhitespaces;
        try {
            Expression left = relationalExpression();
            skipWhitespace();
            if (reader.current().is('&') && reader.next().is('&')) {
                reader.consume(2);
                Expression right = parseExpression(canSkipWhitespace);
                return left;//TODO reOrder(left, right, BinaryOperation.Op.AND);
            }
            if (reader.current().is('|') && reader.next().is('|')) {
                reader.consume(2);
                Expression right = parseExpression(canSkipWhitespace);
                return left;//TODO reOrder(left, right, BinaryOperation.Op.AND);
            }

            skipWhitespace();

            return left;
        } finally {
            canSkipWhitespace = oldSkipWhitespaces;
        }
    }

    private Expression relationalExpression() {
        Expression left = term();
        skipWhitespace();
        if (reader.current().is('<')) {
            reader.consume();
            if (reader.current().is('=')) {
                Expression right = relationalExpression();
                return left;//reOrder(left, right, BinaryOperation.Op.LT);
            }

            Expression right = relationalExpression();
            return left;//reOrder(left, right, BinaryOperation.Op.LT);
        }
/*        if (tokenizer.current().isSymbol("<=")) {
            tokenizer.consume();
            parsii.eval.Expression right = relationalExpression();
            return reOrder(left, right, BinaryOperation.Op.LT_EQ);
        }
        if (tokenizer.current().isSymbol("=")) {
            tokenizer.consume();
            parsii.eval.Expression right = relationalExpression();
            return reOrder(left, right, BinaryOperation.Op.EQ);
        }
        if (tokenizer.current().isSymbol(">=")) {
            tokenizer.consume();
            parsii.eval.Expression right = relationalExpression();
            return reOrder(left, right, BinaryOperation.Op.GT_EQ);
        }
        if (tokenizer.current().isSymbol(">")) {
            tokenizer.consume();
            parsii.eval.Expression right = relationalExpression();
            return reOrder(left, right, BinaryOperation.Op.GT);
        }
        if (tokenizer.current().isSymbol("!=")) {
            tokenizer.consume();
            parsii.eval.Expression right = relationalExpression();
            return reOrder(left, right, BinaryOperation.Op.NEQ);
        }
        */
        return left;
    }

    private Expression term() {
        Expression left = product();
        skipWhitespace();
        if (reader.current().is('+')) {
            reader.consume();
            Expression right = term();
            return left;//reOrder(left, right, BinaryOperation.Op.ADD);
        }
        if (reader.current().is('-')) {
            reader.consume();
            Expression right = term();
            return left; //reOrder(left, right, BinaryOperation.Op.SUBTRACT);
        }

        return left;
    }

    private Expression product() {
        Expression left = chain();
        skipWhitespace();
        if (reader.current().is('*')) {
            reader.consume();
            Expression right = product();
            return left;//reOrder(left, right, BinaryOperation.Op.MULTIPLY);
        }
        if (reader.current().is('/')) {
            reader.consume();
            Expression right = product();
            return left;//reOrder(left, right, BinaryOperation.Op.MULTIPLY);
        }
        if (reader.current().is('%')) {
            reader.consume();
            Expression right = product();
            return left;//reOrder(left, right, BinaryOperation.Op.MULTIPLY);
        }

        return left;
    }

    private Expression chain() {
        Expression root = atom();
        while (!reader.current().isEndOfInput()) {
            skipWhitespace();

            if (!reader.current().is('.')) {
                return root;
            }

            reader.consume();

            skipWhitespace();
            if (!isAtIdentifier()) {
                //TODO fail
            }
            String id = readIdentifier();
            skipWhitespace();
            if (!reader.current().is('(')) {
                //TODO Fail or support variable / constant references!
            }

            root = call(root, id);
        }

        return root;
    }

    private Expression call(Expression self, String methodName) {
        consumeExpectedCharacter('(');
        MethodCall call = new MethodCall(self);
        if (!reader.current().is(')')) {
            List<Expression> parameters = new ArrayList<>();
            while (!reader.current().isEndOfInput()) {
                parameters.add(parseExpression(true));
                if (reader.current().is(')')) {
                    break;
                }
                consumeExpectedCharacter(',');
            }
            call.setParameters(parameters);
        }
        call.bindToMethod(methodName);
        consumeExpectedCharacter(')');
        return call;
    }

    private Expression atom() {
        skipWhitespace();
        if (reader.current().is('(')) {
            reader.consume();
            Expression result = parseExpression(true);
            skipWhitespace();
            consumeExpectedCharacter(')');
            return result;
        }

        if (isAtIdentifier()) {
            String id = readIdentifier();
            skipWhitespace();

            return variable(id);
        }

        return literal();
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

        return null;
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

    private void skipWhitespace() {
        while (canSkipWhitespace && reader.current().isWhitepace()) {
            reader.consume();
        }
    }

    private void consumeExpectedCharacter(char c) {
        if (!reader.current().is(c)) {
            //TODO
        } else {
            reader.consume();
        }
    }
}
