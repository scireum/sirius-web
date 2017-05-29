/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.function.Function;

/**
 * Created by aha on 10.05.17.
 */
public class ConstantEmitter extends Emitter {

    public static final ConstantEmitter EMPTY = new ConstantEmitter(Position.UNKNOWN);

    private String value = "";

    public ConstantEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    @Override
    public Emitter copy() {
        return this;
    }

    @Override
    public Emitter reduce() {
        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        // No internal expressions
    }

    public void append(String stringToAppend) {
        value += stringToAppend;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        context.output(value);
    }

    @Override
    public String toString() {
        return value;
    }
}
