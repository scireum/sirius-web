/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.function.Function;

/**
 * Emits the result of an evaluated expression.
 */
public class ExpressionEmitter extends Emitter {

    private Expression expression;

    /**
     * Creates a new instance at the given position which the given expression.
     *
     * @param startOfBlock the position where the emitter was declared
     * @param expression   the expression to declare at runtime
     */
    public ExpressionEmitter(Position startOfBlock, Expression expression) {
        super(startOfBlock);
        this.expression = expression;
    }

    @Override
    public Emitter copy() {
        return new ExpressionEmitter(startOfBlock, expression.copy());
    }

    /**
     * Reduces the internal expression.
     * <p>
     * Note that we deliberately do not convert a constant expression into a {@link ConstantEmitter} as the escaper
     * might be changed at runtime.
     *
     * @return always returns <tt>this</tt>
     * @see sirius.tagliatelle.rendering.GlobalRenderContext#setEscaper(Function)
     */
    @Override
    public Emitter reduce() {
        this.expression = expression.reduce();

        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        ExpressionVisitor visitor = visitorSupplier.apply(getStartOfBlock());
        this.expression = expression.visit(visitor);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Object value = expression.eval(context);
        if (value != null) {
            context.outputEscaped(value.toString());
        }
    }

    @Override
    public String toString() {
        return "@(" + expression + ")";
    }
}
