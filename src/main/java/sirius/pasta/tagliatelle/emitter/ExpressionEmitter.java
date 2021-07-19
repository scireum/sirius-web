/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.kernel.health.Exceptions;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import java.util.function.UnaryOperator;

/**
 * Emits the result of an evaluated expression.
 */
public class ExpressionEmitter extends Emitter {

    private final Callable expression;

    /**
     * Creates a new instance at the given position with the given expression.
     *
     * @param startOfBlock the position where the emitter was declared
     * @param expression   the expression to declare at runtime
     */
    public ExpressionEmitter(Position startOfBlock, Callable expression) {
        super(startOfBlock);
        this.expression = expression;
    }

    /**
     * Reduces the internal expression.
     * <p>
     * Note that we deliberately do not convert a constant expression into a {@link ConstantEmitter} as the escaper
     * might be changed at runtime.
     *
     * @return always returns <tt>this</tt>
     * @see sirius.pasta.tagliatelle.rendering.GlobalRenderContext#setEscaper(UnaryOperator)
     */
    @Override
    public Emitter reduce() {
        if (this.expression instanceof ConstantCall) {
            try {
                Object value = this.expression.call(null);
                if (value != null) {
                    return new ConstantEmitter(startOfBlock).append(value.toString());
                } else {
                    return ConstantEmitter.EMPTY;
                }
            } catch (ScriptingException e) {
                throw Exceptions.createHandled().withDirectMessage(e.getMessage()).handle();
            }
        }

        return this;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Object value = expression.call(context);
        if (value != null) {
            context.outputEscaped(value.toString());
        }
    }

    @Override
    public String toString() {
        return "@(" + expression + ")";
    }
}
