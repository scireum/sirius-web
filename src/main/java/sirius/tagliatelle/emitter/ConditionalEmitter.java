/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.expression.ConstantBoolean;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;
import sirius.tagliatelle.tags.IfTag;

import java.util.function.Function;

/**
 * Represents a conditional block which is only emitted if a given expression evaluates to <tt>true</tt>.
 * <p>
 * A conditional block can either be defined via the built-in tag &lt;i:if&gt; or via an if statement.
 *
 * @see IfTag
 */
public class ConditionalEmitter extends Emitter {

    protected Expression conditionExpression;
    protected Emitter whenTrue = ConstantEmitter.EMPTY;
    protected Emitter whenFalse = ConstantEmitter.EMPTY;

    /**
     * Creates a new emitter for the given position.
     *
     * @param startOfBlock the position where the conditional block was defined.
     */
    public ConditionalEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        if (conditionExpression == null) {
            return;
        }

        Object condition = conditionExpression.eval(context);
        if (condition != null && (Boolean) condition) {
            whenTrue.emit(context);
        } else {
            whenFalse.emit(context);
        }
    }

    /**
     * Specifies the expression to evaluate to determine which block to emit.
     *
     * @param conditionExpression the condition as expression
     */
    public void setConditionExpression(Expression conditionExpression) {
        this.conditionExpression = conditionExpression;
    }

    /**
     * Specifies the block to emit if the condition evaluates to <tt>true</tt>.
     *
     * @param whenTrue the block to emit when the condition is true
     */
    public void setWhenTrue(Emitter whenTrue) {
        this.whenTrue = whenTrue;
    }

    /**
     * Specifies the block to emit if the condition evaluates to <tt>false</tt>.
     *
     * @param whenFalse the block to emit when the condition is false
     */
    public void setWhenFalse(Emitter whenFalse) {
        if (whenFalse != null) {
            this.whenFalse = whenFalse;
        }
    }

    @Override
    public Emitter copy() {
        ConditionalEmitter copy = new ConditionalEmitter(startOfBlock);
        copy.conditionExpression = conditionExpression.copy();
        copy.whenTrue = whenTrue.copy();
        copy.whenFalse = whenFalse.copy();

        return copy;
    }

    /**
     * Reduces the condition as well as the true and false blocks.
     * <p>
     * If the condition becomes constant, the emitter is reduced the the repective true or false block.
     *
     * @return either the emitter itself or, if the condition is constant, the true or false block
     */
    @Override
    public Emitter reduce() {
        this.conditionExpression = conditionExpression.reduce();
        this.whenTrue = whenTrue.reduce();
        this.whenFalse = whenFalse.reduce();

        if (ConstantBoolean.TRUE.equals(conditionExpression)) {
            return whenTrue;
        }

        if (ConstantBoolean.FALSE.equals(conditionExpression)) {
            return whenFalse;
        }

        return this;
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        ExpressionVisitor visitor = visitorSupplier.apply(getStartOfBlock());
        conditionExpression = conditionExpression.propagateVisitor(visitor);
        whenTrue.visitExpressions(visitorSupplier);
        whenFalse.visitExpressions(visitorSupplier);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("@if (");
        sb.append(conditionExpression);
        sb.append(") {");
        sb.append(whenTrue);
        if (!ConstantEmitter.EMPTY.equals(whenFalse)) {
            sb.append("} else {");
            sb.append(whenFalse);
        }
        sb.append("}");

        return sb.toString();
    }
}
