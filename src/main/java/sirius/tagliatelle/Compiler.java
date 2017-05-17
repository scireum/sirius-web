/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import com.google.common.io.CharStreams;
import parsii.tokenizer.Char;
import parsii.tokenizer.LookaheadReader;
import parsii.tokenizer.ParseError;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConditionalEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.ExpressionEmitter;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;
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

    private final String input;

    public Compiler(CompilationContext context, String input) {
        super(new LookaheadReader(new StringReader(input)), context);
        this.input = input;
    }

    public void compile() throws CompileException {
        if (reader == null) {
            throw new IllegalArgumentException("Reader is null - please do not re-use a Compiler instance.");
        }

        context.getTemplate().emitter = parseBlock(null, null);
        reader = null;

        if (context.hasErrors() || context.hasWarnings()) {
            List<String> lines = getInputAsLines();
            List<CompileError> compileErrors = new ArrayList<>();
            for (ParseError error : context.getErrors()) {
                if (error.getPosition().getLine() >= 1 && error.getPosition().getLine() <= lines.size()) {
                    compileErrors.add(new CompileError(error, lines.get(error.getPosition().getLine() - 1)));
                } else {
                    compileErrors.add(new CompileError(error, null));
                }
            }
            throw CompileException.create(compileErrors);
        }
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

            if ("}".equals(waitFor) && reader.current().is('}')) {
                return block;
            }

            if (reader.current().is('<')) {
                if (waitFor != null && reader.next().is('/')) {
                    if (isAtWaitFor(2, waitFor)) {
                        while (!reader.current().isEndOfInput() && !reader.current().is('>')) {
                            reader.consume();
                        }

                        consumeExpectedCharacter('>');
                        return block;
                    }
                }

                reader.consume();
                String tagName = parseName();
                TagHandler handler = context.findTagHandler(tagName);
                if (handler != null) {
                    handleTag(parentHandler, handler, tagName, block);
                } else {
                    staticText.append("<");
                    staticText.append(tagName);
                    continue;
                }
            }

            if (reader.current().is('@')) {
                reader.consume();
                if (isAtIf()) {
                    block.addChild(parseIf());
                } else if (isAtFor()) {
                    block.addChild(parseForLoop());
                } else {
                    block.addChild(new ExpressionEmitter(reader.current(), parseExpression(false)));
                }
            }

            if (!reader.current().isEndOfInput()) {
                staticText = new ConstantEmitter(reader.current());
                block.addChild(staticText);
            }
        }

        return block;
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

    private void handleTag(TagHandler parentHandler, TagHandler handler, String tagName, CompositeEmitter block) {
        Char startOfTag = reader.current();
        while (true) {
            skipExpectedWhitespace();
            if (reader.current().isEndOfInput() || reader.current().is('>') || reader.current().is('/')) {
                break;
            }
            String name = parseName();
            Class<?> attributeType = handler.getExpectedAttributeType(name);
            if (attributeType == null) {
                context.warning(reader.current(),
                                "Unknown attribute. %s doesn't have an attribute '%s'.",
                                tagName,
                                name);
                attributeType = String.class;
            }

            if (handler.getAttribute(name) != null) {
                context.warning(reader.current(), "Duplicate attribute. A value for '%s' is already present.", name);
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
            if (reader.current().is('}')
                || (reader.current().is('<') && !reader.next().isWhitepace())
                || (reader.current().is('@') && !reader.next().is('@'))) {
                break;
            }

            sb.append(reader.consume().getValue());
        }

        return sb.toString();
    }
}
