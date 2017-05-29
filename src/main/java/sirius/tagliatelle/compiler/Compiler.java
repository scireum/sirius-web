/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import com.google.common.io.CharStreams;
import parsii.tokenizer.Char;
import parsii.tokenizer.LookaheadReader;
import parsii.tokenizer.ParseError;
import parsii.tokenizer.Position;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConditionalEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.ExpressionEmitter;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.MacroCall;
import sirius.tagliatelle.tags.TagContext;
import sirius.tagliatelle.tags.TagHandler;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by aha on 10.05.17.
 */
public class Compiler extends InputProcessor {

    private enum CompilerState {
        PROCEED, STOP, NEW_BLOCK
    }

    private final String input;

    public Compiler(CompilationContext context, String input) {
        super(new LookaheadReader(new StringReader(input)), context);
        this.input = input;
    }

    public List<CompileError> compile() throws CompileException {
        if (reader == null) {
            throw new IllegalArgumentException("Reader is null - please do not re-use a Compiler instance.");
        }

        Emitter emitter = parseBlock(null, null).reduce();
        verifyMacros(emitter);
        context.getTemplate().setEmitter(emitter);

        context.getTemplate().setStackDepth(context.getStackDepth());
        reader = null;

        return processCollectedErrrors();
    }

    private List<CompileError> processCollectedErrrors() throws CompileException {
        if (context.getErrors().isEmpty()) {
            return Collections.emptyList();
        }

        List<CompileError> compileErrors = new ArrayList<>(context.getErrors().size());
        boolean errorFound = false;
        List<String> lines = getInputAsLines();
        for (ParseError error : context.getErrors()) {
            errorFound |= error.getSeverity() == ParseError.Severity.ERROR;

            if (error.getPosition().getLine() >= 1 && error.getPosition().getLine() <= lines.size()) {
                compileErrors.add(new CompileError(error, lines.get(error.getPosition().getLine() - 1)));
            } else {
                compileErrors.add(new CompileError(error, null));
            }
        }

        if (errorFound) {
            throw CompileException.create(context.getTemplate(), compileErrors);
        }

        return compileErrors;
    }

    private void verifyMacros(Emitter emitter) {
        emitter.visitExpressions(p -> e -> verifyMacro(p, e));
    }

    private Expression verifyMacro(Position pos, Expression expr) {
        if (expr instanceof MacroCall) {
            try {
                ((MacroCall) expr).verify();
            } catch (IllegalArgumentException ex) {
                context.error(pos, "Invalid parameters for macro: %s: %s", expr, ex.getMessage());
            }
        }

        return expr;
    }

    private List<String> getInputAsLines() {
        try {
            return CharStreams.readLines(new StringReader(input));
        } catch (IOException e) {
            Exceptions.ignore(e);
        }

        return Collections.emptyList();
    }

    private CompositeEmitter parseBlock(TagHandler parentHandler, String waitFor) {
        CompositeEmitter block = new CompositeEmitter(reader.current());
        ConstantEmitter staticText = new ConstantEmitter(reader.current());
        block.addChild(staticText);

        while (!reader.current().isEndOfInput()) {
            staticText.append(consumeStaticBlock());

            if (checkForEndOfBlock(waitFor, staticText)) {
                return block;
            }

            if (processTag(parentHandler, block, staticText)) {
                if (!reader.current().isEndOfInput()) {
                    staticText = new ConstantEmitter(reader.current());
                    block.addChild(staticText);
                }
            }

            if (processExpression(block)) {
                if (!reader.current().isEndOfInput()) {
                    staticText = new ConstantEmitter(reader.current());
                    block.addChild(staticText);
                }
            }
        }

        ensureBlockCompletion(waitFor, block);

        return block;
    }

    private void ensureBlockCompletion(String waitFor, CompositeEmitter block) {
        if ("}".equals(waitFor)) {
            context.error(block.getStartOfBlock(), "Missing closing } for this block");
        } else if (waitFor != null) {
            context.error(block.getStartOfBlock(), "Missing closing tag for this block");
        }
    }

    private boolean checkForEndOfBlock(String waitFor, ConstantEmitter staticText) {
        if (waitFor == null) {
            return false;
        }

        if (reader.current().is('}')) {
            if ("}".equals(waitFor)) {
                return true;
            }
        }

        if (reader.current().is('<') && reader.next().is('/')) {
            if (isAtWaitFor(2, waitFor)) {
                while (!reader.current().isEndOfInput() && !reader.current().is('>')) {
                    reader.consume();
                }

                consumeExpectedCharacter('>');
                return true;
            }
        }

        staticText.append(reader.consume().getStringValue());

        return false;
    }

    private boolean isAtWaitFor(int initialOffset, String waitFor) {
        int offset = initialOffset;
        while (reader.next(offset).isWhitepace()) {
            offset++;
        }

        return isAtText(offset, waitFor);
    }

    private boolean isAtText(int offset, String text) {
        for (int i = 0; i < text.length(); i++) {
            if (!reader.next(offset + i).is(text.charAt(i))) {
                return false;
            }
        }

        return true;
    }

    private boolean processTag(TagHandler parentHandler, CompositeEmitter block, ConstantEmitter staticText) {
        if (!reader.current().is('<')) {
            return false;
        }

        reader.consume();
        String tagName = parseName();
        TagHandler handler = context.findTagHandler(reader.current(), tagName);
        if (handler != null) {
            handleTag(parentHandler, handler, tagName, block);
            return true;
        }

        staticText.append("<");
        staticText.append(tagName);
        return false;
    }

    private boolean processExpression(CompositeEmitter block) {
        if (!reader.current().is('@')) {
            return false;
        }
        reader.consume();
        if (isAtIf()) {
            block.addChild(parseIf());
        } else if (isAtFor()) {
            block.addChild(parseForLoop());
        } else {
            block.addChild(new ExpressionEmitter(reader.current(), parseExpression(false)));
        }

        return true;
    }

    private void handleTag(TagHandler parentHandler, TagHandler handler, String tagName, CompositeEmitter block) {
        Char startOfTag = reader.current();
        parseAttributes(handler, tagName);

        boolean selfClosed = reader.current().is('/');
        if (selfClosed) {
            consumeExpectedCharacter('/');
            skipUnexpectedWhitespace();
            consumeExpectedCharacter('>');
        } else {
            consumeExpectedCharacter('>');
            CompositeEmitter body = parseBlock(handler, tagName);
            handler.addBlock("body", body);
        }

        handler.apply(new TagContext(startOfTag, context, parentHandler, block));
    }

    private void parseAttributes(TagHandler handler, String tagName) {
        while (true) {
            skipExpectedWhitespace();
            if (reader.current().isEndOfInput() || reader.current().is('>') || reader.current().is('/')) {
                break;
            }
            String name = parseName();
            Class<?> attributeType = handler.getExpectedAttributeType(name);
            verifyAttributeNameAndType(handler, tagName, name, attributeType);
            if (attributeType == null) {
                attributeType = String.class;
            }

            skipUnexpectedWhitespace();
            consumeExpectedCharacter('=');
            skipUnexpectedWhitespace();
            Char positionOfAttribute = reader.current();
            consumeExpectedCharacter('"');
            skipUnexpectedWhitespace();

            if (reader.current().is('"')) {
                warnForInvalidExpressionType(positionOfAttribute,
                                             tagName,
                                             name,
                                             attributeType,
                                             ConstantNull.NULL.getType());
                handler.setAttribute(name, ConstantNull.NULL);
            } else if (reader.current().is('@')) {
                reader.consume();
                Expression expression = parseExpression(true);
                warnForInvalidExpressionType(positionOfAttribute, tagName, name, attributeType, expression.getType());
                handler.setAttribute(name, expression);
            } else if (!String.class.equals(attributeType)) {
                Expression expression = parseExpression(true);
                warnForInvalidExpressionType(positionOfAttribute, tagName, name, attributeType, expression.getType());
                handler.setAttribute(name, expression);
            } else {
                warnForInvalidExpressionType(positionOfAttribute, tagName, name, attributeType, String.class);
                handler.setAttribute(name, parseAttributeValue());
            }
            consumeExpectedCharacter('"');
        }
    }

    private void verifyAttributeNameAndType(TagHandler handler,
                                            String tagName,
                                            String attributeName,
                                            Class<?> attributeType) {
        if (attributeType == null) {
            context.warning(reader.current(),
                            "Unknown attribute. %s doesn't have an attribute '%s'.",
                            tagName,
                            attributeName);
        }

        if (handler.getAttribute(attributeName) != null) {
            context.warning(reader.current(),
                            "Duplicate attribute. A value for '%s' is already present.",
                            attributeName);
        }
    }

    private void warnForInvalidExpressionType(Char positionOfAttribute,
                                              String tagName,
                                              String name,
                                              Class<?> expectedType,
                                              Class<?> actualType) {
        if (expectedType == Expression.class) {
            return;
        }

        if (!expectedType.isAssignableFrom(actualType)) {
            context.error(positionOfAttribute,
                          "Incompatible attribute types. %s expects %s for '%s', but %s was given.",
                          tagName,
                          expectedType,
                          name,
                          actualType);
        }
    }

    private Expression parseAttributeValue() {
        StringBuilder sb = new StringBuilder();
        while (!reader.current().isEndOfInput() && !reader.current().is('"')) {
            sb.append(reader.consume().getValue());
        }

        return new ConstantString(sb.toString());
    }

    private String parseName() {
        StringBuilder sb = new StringBuilder();
        while (reader.current().isDigit() || reader.current().isLetter() || reader.current().is('.', '-', '_', ':')) {
            sb.append(reader.consume().getValue());
        }
        return sb.toString();
    }

    private Emitter parseForLoop() {
        return null;
    }

    private Emitter parseIf() {
        ConditionalEmitter result = new ConditionalEmitter(reader.current());
        reader.consume(2);
        skipExpectedWhitespace();
        consumeExpectedCharacter('(');
        result.setConditionExpression(parseExpression(true));
        skipUnexpectedWhitespace();
        consumeExpectedCharacter(')');
        skipExpectedWhitespace();
        consumeExpectedCharacter('{');
        result.setWhenTrue(parseBlock(null, "}"));
        consumeExpectedCharacter('}');
        if (isAtElse()) {
            skipExpectedWhitespace();
            reader.consume(4);
            skipExpectedWhitespace();
            consumeExpectedCharacter('{');
            result.setWhenFalse(parseBlock(null, "}"));
            consumeExpectedCharacter('}');
        }

        return result;
    }

    private Expression parseExpression(boolean skipWhitespaces) {
        return new Parser(reader, context).parse(skipWhitespaces);
    }

    private boolean isAtFor() {
        return isAtText(0, "for");
    }

    private boolean isAtIf() {
        return isAtText(0, "if");
    }

    private boolean isAtElse() {
        int offset = 0;
        while (reader.next(offset).isWhitepace()) {
            offset++;
        }
        return isAtText(offset, "else");
    }

    private String consumeStaticBlock() {
        StringBuilder sb = new StringBuilder();
        while (!reader.current().isEndOfInput()) {
            if (isAtEscapedAt()) {
                sb.append(reader.consume().getValue());
                reader.consume().getValue();
            } else {
                if (isAtPotentialEndOfStaticBlock()) {
                    return sb.toString();
                }

                sb.append(reader.consume().getValue());
            }
        }

        return sb.toString();
    }

    private boolean isAtEscapedAt() {
        return reader.current().is('@') && reader.next().is('@');
    }

    private boolean isAtPotentialEndOfStaticBlock() {
        if (reader.current().is('}')) {
            return true;
        }

        if (reader.current().is('@')) {
            return true;
        }

        return reader.current().is('<') && !reader.next().isWhitepace();
    }
}
