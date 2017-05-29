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

import java.util.function.Function;

/**
 * Created by aha on 10.05.17.
 */
public class ConditionalEmitter extends Emitter {

    protected Expression conditionExpression;
    protected Emitter whenTrue = ConstantEmitter.EMPTY;
    protected Emitter whenFalse = ConstantEmitter.EMPTY;

    public ConditionalEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Object condition = conditionExpression.eval(context);
        if (condition != null && (Boolean) condition) {
            whenTrue.emit(context);
        } else {
            whenFalse.emit(context);
        }
    }

    public Expression getConditionExpression() {
        return conditionExpression;
    }

    public void setConditionExpression(Expression conditionExpression) {
        this.conditionExpression = conditionExpression;
    }

    public Emitter getWhenTrue() {
        return whenTrue;
    }

    public void setWhenTrue(Emitter whenTrue) {
        this.whenTrue = whenTrue;
    }

    public Emitter getWhenFalse() {
        return whenFalse;
    }

    public void setWhenFalse(Emitter whenFalse) {
        this.whenFalse = whenFalse;
    }

    @Override
    public Emitter copy() {
        ConditionalEmitter copy = new ConditionalEmitter(startOfBlock);
        copy.conditionExpression = conditionExpression.copy();
        copy.whenTrue = whenTrue.copy();
        copy.whenFalse = whenFalse.copy();

        return copy;
    }

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
    public Emitter visit(EmitterVisitor visitor) {
        this.whenTrue = whenTrue.visit(visitor);
        this.whenFalse = whenFalse.visit(visitor);
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        ExpressionVisitor visitor = visitorSupplier.apply(getStartOfBlock());
        conditionExpression = conditionExpression.visit(visitor);
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
