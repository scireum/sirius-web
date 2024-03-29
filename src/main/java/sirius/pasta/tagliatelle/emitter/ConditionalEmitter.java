/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.health.Exceptions;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

/**
 * Represents a conditional block which is only emitted if a given expression evaluates to <tt>true</tt>.
 * <p>
 * A conditional block can either be defined via the built-in tag &lt;i:if&gt; or via an if statement.
 *
 * @see sirius.pasta.tagliatelle.tags.IfTag
 */
public class ConditionalEmitter extends Emitter {

    protected Callable conditionExpression;
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

        Object condition = conditionExpression.call(context);
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
    public void setConditionExpression(Callable conditionExpression) {
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

    /**
     * Reduces the condition as well as the true and false blocks.
     * <p>
     * If the condition becomes constant, the emitter is reduced the the respective true or false block.
     *
     * @return either the emitter itself or, if the condition is constant, the true or false block
     */
    @Override
    public Emitter reduce() {
        this.whenTrue = whenTrue.reduce();
        this.whenFalse = whenFalse.reduce();

        if (conditionExpression instanceof ConstantCall) {
            try {
                if (Boolean.TRUE.equals(conditionExpression.call(null))) {
                    return whenTrue;
                } else {
                    return whenFalse;
                }
            } catch (ScriptingException e) {
                Exceptions.ignore(e);
            }
        }

        return this;
    }

    /**
     * Verifies the consistency of the emitter.
     *
     * @param compilationContext the context used to report errors to
     */
    public void verify(CompilationContext compilationContext) {
        if (!CompilationContext.isAssignableTo(conditionExpression.getType(), Boolean.class)) {
            compilationContext.error(startOfBlock, "An if must have a boolean as condition");
        }
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
