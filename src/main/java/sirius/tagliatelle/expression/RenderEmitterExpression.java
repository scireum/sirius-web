/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.rendering.LocalRenderContext;

/**
 * Returns the render result of an emitter as string.
 * <p>
 * This is used when a call to {@link sirius.tagliatelle.macros.RenderBlockMacro} is inlined.
 */
public class RenderEmitterExpression implements Expression {

    private Emitter emitter;

    /**
     * Creates an expression which evaluates to the result of the given emitter.
     *
     * @param emitter the emitter to wrap
     */
    public RenderEmitterExpression(Emitter emitter) {
        this.emitter = emitter;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return ctx.getGlobalContext().emitToString(() -> {
            emitter.emit(ctx);
        });
    }

    @Override
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        return visitor.visitThis(this);
    }

    @Override
    public Expression reduce() {
        Emitter reducedEmitter = emitter.reduce();

        if (reducedEmitter instanceof ConstantEmitter) {
            return new ConstantString(((ConstantEmitter) reducedEmitter).getValue().trim());
        }

        return this;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Expression copy() {
        return new RenderEmitterExpression(emitter);
    }

    @Override
    public Class<?> getType() {
        return String.class;
    }
}
