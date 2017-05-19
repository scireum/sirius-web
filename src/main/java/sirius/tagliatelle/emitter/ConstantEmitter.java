/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import com.google.common.base.Charsets;
import parsii.tokenizer.Position;
import sirius.tagliatelle.LocalRenderContext;
import sirius.tagliatelle.expression.ExpressionVisitor;

/**
 * Created by aha on 10.05.17.
 */
public class ConstantEmitter extends Emitter {

    public static final ConstantEmitter EMPTY = new ConstantEmitter(Position.UNKNOWN);

    private String value = "";
    private byte[] valueAsBytes;

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
    public void visitExpressions(ExpressionVisitor visitor) {

    }

    public void append(String stringToAppend) {
        value += stringToAppend;
        valueAsBytes = null;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        if (context.isAcceptingBytes()) {
            if (valueAsBytes == null) {
                valueAsBytes = value.getBytes(Charsets.UTF_8);
            }
            context.output(valueAsBytes);
            return;
        }

        context.output(value);
    }

    @Override
    public String toString() {
        return value;
    }
}
