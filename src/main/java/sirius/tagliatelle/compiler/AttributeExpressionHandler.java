/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import org.jetbrains.annotations.NotNull;
import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConditionalEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.ExpressionEmitter;
import sirius.tagliatelle.emitter.PushLocalEmitter;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.EqualsOperation;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ReadLocal;

/**
 * Parses an attribute expression.
 * <p>
 * This is replaces {@literal @}if (x) { y="y" } with {@literal @}y="x".
 */
@Register(classes = ExpressionHandler.class)
public class AttributeExpressionHandler extends ExpressionHandler {

    @Override
    public int getPriority() {
        return 900;
    }

    @Override
    public boolean shouldProcess(Compiler compiler) {
        int offset = 0;
        while (compiler.getReader().next(offset).isLetter()) {
            offset++;
        }
        return compiler.getReader().next(offset).is('=') && compiler.getReader().next(offset + 1).is('"');
    }

    @Override
    public Emitter process(Compiler compiler) {
        Position start = compiler.getReader().current();
        String attributeName = readAttributeName(compiler);

        compiler.consumeExpectedCharacter('=');
        compiler.consumeExpectedCharacter('"');
        compiler.skipWhitespaces();
        if (compiler.getReader().current().is('@')) {
            compiler.getReader().consume();
            compiler.skipWhitespaces();
        }

        Expression condition = compiler.parseExpression(true);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('"');
        if (boolean.class.equals(condition.getType())) {
            return translateBooleanExpression(start, attributeName, condition);
        }

        return translateLiteralExpression(compiler, start, attributeName, condition);
    }

    @NotNull
    private Emitter translateLiteralExpression(Compiler compiler,
                                               Position start,
                                               String attributeName,
                                               Expression literalExpression) {
        CompositeEmitter result = new CompositeEmitter(start);

        Expression constantLiteralExpression = ensureConstantExpression(compiler, start, literalExpression, result);

        ConditionalEmitter conditionalEmitter = new ConditionalEmitter(start);
        conditionalEmitter.setConditionExpression(new EqualsOperation(constantLiteralExpression,
                                                                      ConstantNull.NULL,
                                                                      true));
        CompositeEmitter trueBlock = new CompositeEmitter(start);
        trueBlock.addChild(new ConstantEmitter(start).append(attributeName).append("=\""));
        trueBlock.addChild(new ExpressionEmitter(start, constantLiteralExpression));
        trueBlock.addChild(new ConstantEmitter(start).append("\""));
        conditionalEmitter.setWhenTrue(trueBlock);
        result.addChild(conditionalEmitter);

        return result;
    }

    private Expression ensureConstantExpression(Compiler compiler,
                                                Position start,
                                                Expression literalExpression,
                                                CompositeEmitter compositeEmitter) {
        if (!literalExpression.isConstant()) {
            return literalExpression;
        }

        int localIndex = compiler.getContext().push(start, null, literalExpression.getType());
        compositeEmitter.addChild(new PushLocalEmitter(start, localIndex, literalExpression));
        return new ReadLocal(literalExpression.getType(), localIndex);
    }

    private Emitter translateBooleanExpression(Position start, String attributeName, Expression condition) {
        ConditionalEmitter result = new ConditionalEmitter(start);
        result.setConditionExpression(condition);
        result.setWhenTrue(new ConstantEmitter(start).append(attributeName)
                                                     .append("=\"")
                                                     .append(attributeName)
                                                     .append("\""));
        return result;
    }

    private String readAttributeName(Compiler compiler) {
        StringBuilder attributeName = new StringBuilder();
        while (compiler.getReader().current().isLetter()) {
            attributeName.append(compiler.getReader().consume());
        }
        return attributeName.toString();
    }
}
