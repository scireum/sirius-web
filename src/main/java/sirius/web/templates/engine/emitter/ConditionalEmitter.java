/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.emitter;

import sirius.web.templates.engine.RenderContext;
import sirius.web.templates.engine.expression.Expression;

/**
 * Created by aha on 10.05.17.
 */
public class ConditionalEmitter implements Emitter {

    protected Expression conditionExpression;
    protected Emitter whenTrue = ConstantEmitter.EMPTY;
    protected Emitter whenFalse = ConstantEmitter.EMPTY;

    @Override
    public void emit(RenderContext context) {
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
}
