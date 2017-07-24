/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.function.Function;

/**
 * Loops over a given {@link Iterable} and invokes the given block for each item within.
 */
public class LoopEmitter extends PushEmitter {

    private Expression iterableExpression;
    private Emitter loop;

    /**
     * Creates a new emitter for the given position.
     *
     * @param startOfBlock the position where the emitter was declared.
     */
    public LoopEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    /**
     * Specifies the expression to evaluate which yields the items to iterate over.
     *
     * @param iterableExpression the expression which evaluates to the list of items to process
     */
    public void setIterableExpression(Expression iterableExpression) {
        this.iterableExpression = iterableExpression;
    }

    /**
     * Sets the body to emit once per item in the list of items to output.
     *
     * @param loop the loop body to emit per item
     */
    public void setLoop(Emitter loop) {
        this.loop = loop;
    }

    @Override
    public Emitter copy() {
        LoopEmitter copy = new LoopEmitter(startOfBlock);
        copy.iterableExpression = iterableExpression.copy();
        copy.loop = loop.copy();
        copy.localIndex = localIndex;

        return copy;
    }

    @Override
    public Emitter reduce() {
        this.loop = loop.reduce();
        this.iterableExpression = iterableExpression.reduce();

        return this;
    }

    /**
     * Verifies the consistency of the emitter.
     *
     * @param compilationContext the context used to report errors to
     */
    public void verify(CompilationContext compilationContext) {
        if (!Iterable.class.isAssignableFrom(iterableExpression.getType())) {
            compilationContext.error(startOfBlock, "A for loop must have an Iterable as expression");
        }
    }

    @Override
    public Emitter propagateVisitor(EmitterVisitor visitor) {
        this.loop = loop.propagateVisitor(visitor);
        return visitor.visitThis(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        ExpressionVisitor visitor = visitorSupplier.apply(getStartOfBlock());
        this.iterableExpression = iterableExpression.propagateVisitor(visitor);
        this.loop.visitExpressions(visitorSupplier);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        if (iterableExpression == null || localIndex < 0) {
            return;
        }

        Object iterable = iterableExpression.eval(context);
        if (iterable == null) {
            return;
        }

        for (Object obj : (Iterable<?>) iterable) {
            context.setLocal(localIndex, obj);
            loop.emit(context);
        }
    }

    /**
     * Returns the expression which yields the items to iterate through.
     *
     * @return the expression to iterate through
     */
    public Expression getIterableExpression() {
        return iterableExpression;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("@for (");
        sb.append(iterableExpression);
        sb.append(") {");
        sb.append(loop);
        sb.append("}");

        return sb.toString();
    }

}
