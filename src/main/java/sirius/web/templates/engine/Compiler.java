/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import parsii.tokenizer.LookaheadReader;
import sirius.web.templates.engine.emitter.CompositeEmitter;
import sirius.web.templates.engine.emitter.ConditionalEmitter;
import sirius.web.templates.engine.emitter.ConstantEmitter;
import sirius.web.templates.engine.emitter.Emitter;
import sirius.web.templates.engine.emitter.ExpressionEmitter;
import sirius.web.templates.engine.expression.Expression;

import java.io.Reader;

/**
 * Created by aha on 10.05.17.
 */
public class Compiler {

    private LookaheadReader reader;
    private CompilationContext context;

    public Compiler(Reader input, CompilationContext context) {
        this.reader = new LookaheadReader(input);
        this.context = context;
    }

    public Template compile() {
        if (reader == null) {
            throw new IllegalArgumentException("Reader is null - please do not re-use a Compiler instance.");
        }

        Template result = new Template();
        CompositeEmitter block = parseBlock(null);

        result.emitter = block;
        reader = null;
        return result;
    }

    private CompositeEmitter parseBlock(String waitFor) {
        CompositeEmitter block = new CompositeEmitter();
        ConstantEmitter staticText = new ConstantEmitter();
        block.addChild(staticText);

        while (!reader.current().isEndOfInput()) {
            staticText.append(consumeStaticBlock());

            if ("}".equals(waitFor) && reader.current().is('}')) {
                return block;
            }

            if (reader.current().is('@')) {
                reader.consume();
                if (isAtIf()) {
                    block.addChild(parseIf());
                } else if (isAtFor()) {
                    block.addChild(parseForLoop());
                } else {
                    block.addChild(new ExpressionEmitter(parseExpression()));
                }
            }

            if (!reader.current().isEndOfInput()) {
                staticText = new ConstantEmitter();
                block.addChild(staticText);
            }
        }

        return block;
    }

    private Emitter parseForLoop() {
        return null;
    }

    private Emitter parseIf() {
        ConditionalEmitter result = new ConditionalEmitter();
        reader.consume(2);
        skipWhitespace();
        consumeExpectedCharacter('(');
        result.setConditionExpression(parseExpression());
        skipWhitespace();
        consumeExpectedCharacter(')');
        skipWhitespace();
        consumeExpectedCharacter('{');
        result.setWhenTrue(parseBlock("}"));
        consumeExpectedCharacter('}');
        skipWhitespace();
        if (isAtElse()) {
            reader.consume(4);
            skipWhitespace();
            consumeExpectedCharacter('{');
            result.setWhenFalse(parseBlock("}"));
            consumeExpectedCharacter('}');
        }

        return result;
    }

    private void consumeExpectedCharacter(char c) {
        if (!reader.current().is(c)) {
            //TODO
        } else {
            reader.consume();
        }
    }

    private void skipWhitespace() {
        while (reader.current().isWhitepace()) {
            reader.consume();
        }
    }

    private Expression parseExpression() {
        return new Parser(reader, context).parse();
    }

    private boolean isAtFor() {
        return reader.current().is('f') && reader.next().is('o') && reader.next(2).is('r');
    }

    private boolean isAtIf() {
        return reader.current().is('i') && reader.next().is('f');
    }

    private boolean isAtElse() {
        return reader.current().is('e') && reader.next().is('l') && reader.next(2).is('s') && reader.next(3).is('e');
    }

    private String consumeStaticBlock() {
        StringBuilder sb = new StringBuilder();
        while (!reader.current().isEndOfInput()) {
            if (reader.current().is('}') || reader.current().is('<') || (reader.current().is('@') && !reader.next()
                                                                                                            .is('@'))) {
                break;
            }

            sb.append(reader.consume().getValue());
        }

        return sb.toString();
    }
}
