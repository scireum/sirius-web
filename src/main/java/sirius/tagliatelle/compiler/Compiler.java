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
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.MacroCall;
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

    public CompilationContext getContext() {
        return context;
    }

    private enum CompilerState {
        PROCEED, STOP, NEW_BLOCK
    }

    private final String input;

    @PriorityParts(ExpressionHandler.class)
    private static List<ExpressionHandler> expressionHandlers;

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

    public CompositeEmitter parseBlock(TagHandler parentHandler, String waitFor) {
        CompositeEmitter block = new CompositeEmitter(reader.current());
        ConstantEmitter staticText = new ConstantEmitter(reader.current());
        block.addChild(staticText);

        while (!reader.current().isEndOfInput()) {
            staticText.append(consumeStaticBlock());

            if (checkForEndOfBlock(waitFor, staticText)) {
                return block;
            }

            if (processTag(parentHandler, block, staticText) || processExpression(block)) {
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
        if (reader.current().is('}')) {
            if ("}".equals(waitFor)) {
                return true;
            } else {
                staticText.append(reader.consume().getStringValue());
            }
        }

        if (waitFor != null && reader.current().is('<') && reader.next().is('/')) {
            if (isAtWaitFor(2, waitFor)) {
                while (!reader.current().isEndOfInput() && !reader.current().is('>')) {
                    reader.consume();
                }

                parseEndOfTag();
                return true;
            } else {
                staticText.append(reader.consume().getStringValue());
            }
        }

        return false;
    }

    private void parseEndOfTag() {
        consumeExpectedCharacter('>');
        if (reader.current().isNewLine()) {
            reader.consume();
        }
    }

    private boolean isAtWaitFor(int initialOffset, String waitFor) {
        int offset = initialOffset;
        while (reader.next(offset).isWhitepace()) {
            offset++;
        }

        return isAtText(offset, waitFor);
    }

    private boolean processTag(TagHandler parentHandler, CompositeEmitter block, ConstantEmitter staticText) {
        if (!reader.current().is('<')) {
            return false;
        }
        Position startOfTag = reader.current();
        reader.consume();
        String tagName = parseName();
        TagHandler handler = context.findTagHandler(reader.current(), tagName);
        if (handler != null) {
            handler.setStartOfTag(startOfTag);
            handler.setParentHandler(parentHandler);
            handler.setCompilationContext(context);
            handler.setTagName(tagName);
            handleTag(handler, block);
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

        for (ExpressionHandler handler : expressionHandlers) {
            if (handler.shouldProcess(this)) {
                Emitter child = handler.process(this);
                if (child != null) {
                    block.addChild(child);
                }
                return true;
            }
        }

        return false;
    }

    private void handleTag(TagHandler handler, CompositeEmitter block) {
        parseAttributes(handler);

        handler.beforeBody();

        boolean selfClosed = reader.current().is('/');
        if (selfClosed) {
            consumeExpectedCharacter('/');
            skipWhitespaces();
            parseEndOfTag();
        } else {
            parseEndOfTag();
            CompositeEmitter body = parseBlock(handler, handler.getTagName());
            handler.addBlock("body", body);
        }

        handler.apply(block);
    }

    private void parseAttributes(TagHandler handler) {
        while (true) {
            skipWhitespaces();
            if (reader.current().isEndOfInput() || reader.current().is('>') || reader.current().is('/')) {
                break;
            }
            String name = parseName();
            Class<?> attributeType = handler.getExpectedAttributeType(name);
            verifyAttributeNameAndType(handler, name, attributeType);
            if (attributeType == null) {
                attributeType = String.class;
            }

            skipWhitespaces();
            consumeExpectedCharacter('=');
            skipWhitespaces();
            Char positionOfAttribute = reader.current();
            consumeExpectedCharacter('"');
            skipWhitespaces();

            if (reader.current().is('"')) {
                warnForInvalidExpressionType(positionOfAttribute,
                                             handler.getTagName(),
                                             name,
                                             attributeType,
                                             ConstantNull.NULL.getType());
                handler.setAttribute(name, ConstantNull.NULL);
            } else if (reader.current().is('@')) {
                reader.consume();
                Expression expression = parseExpression(true);
                warnForInvalidExpressionType(positionOfAttribute,
                                             handler.getTagName(),
                                             name,
                                             attributeType,
                                             expression.getType());
                handler.setAttribute(name, expression);
            } else if (!String.class.equals(attributeType)) {
                Expression expression = parseExpression(true);
                warnForInvalidExpressionType(positionOfAttribute,
                                             handler.getTagName(),
                                             name,
                                             attributeType,
                                             expression.getType());
                handler.setAttribute(name, expression);
            } else {
                warnForInvalidExpressionType(positionOfAttribute,
                                             handler.getTagName(),
                                             name,
                                             attributeType,
                                             String.class);
                handler.setAttribute(name, parseAttributeValue());
            }
            consumeExpectedCharacter('"');
        }
    }

    private void verifyAttributeNameAndType(TagHandler handler, String attributeName, Class<?> attributeType) {
        if (attributeType == null) {
            context.warning(reader.current(),
                            "Unknown attribute. %s doesn't have an attribute '%s'.",
                            handler.getTagName(),
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

    public Expression parseExpression(boolean skipWhitespaces) {
        return new Parser(reader, context).parse(skipWhitespaces);
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
