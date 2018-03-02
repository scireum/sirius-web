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
            ConditionalEmitter result = new ConditionalEmitter(start);
            result.setConditionExpression(condition);
            result.setWhenTrue(new ConstantEmitter(start).append(attributeName)
                                                         .append("=\"")
                                                         .append(attributeName)
                                                         .append("\""));
            return result;
        }

        CompositeEmitter result = new CompositeEmitter(start);
        if (!condition.isConstant()) {
            PushLocalEmitter localEmitter = new PushLocalEmitter(start,
                                                                 compiler.getContext()
                                                                         .push(start, null, condition.getType()),
                                                                 condition);
            result.addChild(localEmitter);
            condition = new ReadLocal(condition.getType(), localEmitter.getLocalIndex());
        }

        Expression effectiveCondition = new EqualsOperation(condition, ConstantNull.NULL, true);

        ConditionalEmitter conditionalEmitter = new ConditionalEmitter(start);
        conditionalEmitter.setConditionExpression(effectiveCondition);
        CompositeEmitter trueBlock = new CompositeEmitter(start);
        trueBlock.addChild(new ConstantEmitter(start).append(attributeName).append("=\""));
        trueBlock.addChild(new ExpressionEmitter(start, condition));
        trueBlock.addChild(new ConstantEmitter(start).append("\""));
        conditionalEmitter.setWhenTrue(trueBlock);
        result.addChild(conditionalEmitter);

        return result;
    }

    @NotNull
    private String readAttributeName(Compiler compiler) {
        StringBuilder attributeName = new StringBuilder();
        while (compiler.getReader().current().isLetter()) {
            attributeName.append(compiler.getReader().consume());
        }
        return attributeName.toString();
    }
}
