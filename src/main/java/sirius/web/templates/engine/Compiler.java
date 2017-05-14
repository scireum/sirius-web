/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import com.google.common.io.CharStreams;
import parsii.tokenizer.LookaheadReader;
import parsii.tokenizer.ParseError;
import sirius.kernel.health.Exceptions;
import sirius.web.templates.engine.emitter.CompositeEmitter;
import sirius.web.templates.engine.emitter.ConditionalEmitter;
import sirius.web.templates.engine.emitter.ConstantEmitter;
import sirius.web.templates.engine.emitter.Emitter;
import sirius.web.templates.engine.emitter.ExpressionEmitter;
import sirius.web.templates.engine.expression.ConstantString;
import sirius.web.templates.engine.expression.Expression;
import sirius.web.templates.engine.tags.TagHandler;

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

    public Compiler(String input, CompilationContext context) {
        super(new LookaheadReader(new StringReader(input)), context);
        this.input = input;
    }

    public Template compile() throws CompileException {
        if (reader == null) {
            throw new IllegalArgumentException("Reader is null - please do not re-use a Compiler instance.");
        }

        Template result = new Template();

        result.emitter = parseBlock(null, null);
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

        return result;
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
        CompositeEmitter block = new CompositeEmitter();
        ConstantEmitter staticText = new ConstantEmitter();
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
                    continue;
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
                    block.addChild(new ExpressionEmitter(parseExpression(false)));
                }
            }

            if (!reader.current().isEndOfInput()) {
                staticText = new ConstantEmitter();
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

    private boolean isHandled(String tagName) {
        return "i:if".equals(tagName);
    }

    private void handleTag(TagHandler parentHandler, TagHandler handler, String tagName, CompositeEmitter block) {
        while (true) {
            skipExpectedWhitespace();
            if (reader.current().isEndOfInput() || reader.current().is('>') || reader.current().is('/')) {
                break;
            }
            String name = parseName();
            skipUnexpectedWhitespace();
            consumeExpectedCharacter('=');
            skipUnexpectedWhitespace();
            consumeExpectedCharacter('"');
            if (reader.current().is('@')) {
                reader.consume();
                handler.setAttribute(name, parseExpression(false));
            } else {
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
            handler.setBody(body);
            //TODO add template as paremter here - or even better - add parameter object
        }

        handler.apply(parentHandler, block);
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
        ConditionalEmitter result = new ConditionalEmitter();
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
