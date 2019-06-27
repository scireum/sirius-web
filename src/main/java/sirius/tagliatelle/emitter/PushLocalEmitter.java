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
 * Writes the result of an expression evaluation into a temporary / local variable.
 */
public class PushLocalEmitter extends Emitter {

    private Expression expression;
    private int localIndex;

    /**
     * Creates a new instance at the given position which writes the value of the given expression into the given local
     * location.
     *
     * @param startOfBlock the position where the statement was created
     * @param localIndex   the local index within the {@link LocalRenderContext} to store the value to
     * @param expression   the expression to evaluate at runtime
     */
    public PushLocalEmitter(Position startOfBlock, int localIndex, Expression expression) {
        super(startOfBlock);
        this.localIndex = localIndex;
        this.expression = expression;
    }

    @Override
    public Emitter copy() {
        return new PushLocalEmitter(startOfBlock, localIndex, expression.copy());
    }

    @Override
    public Emitter reduce() {
        this.expression = expression.reduce();

        return this;
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        ExpressionVisitor visitor = visitorSupplier.apply(getStartOfBlock());
        this.expression = expression.propagateVisitor(visitor);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Object value = expression.eval(context);
        context.setLocal(localIndex, value);
    }

    @Override
    public String toString() {
        return "LOCAL<" + localIndex + "> = @(" + expression + ")";
    }

    /**
     * Contains the expression to evaluate and save to the stack location.
     *
     * @return the expression to evaluate
     */
    public Expression getExpression() {
        return expression;
    }

    /**
     * Contains the stack index being written to.
     *
     * @return the target index to write to
     */
    public int getLocalIndex() {
        return localIndex;
    }

    /**
     * Updates the stack index being written to.
     * <p>
     * When inlining a template, the stack has to be transferred to the callee and therefore the
     * stack indices might change.
     *
     * @param localIndex the new stack index to use
     */
    public void setLocalIndex(int localIndex) {
        this.localIndex = localIndex;
    }
}
