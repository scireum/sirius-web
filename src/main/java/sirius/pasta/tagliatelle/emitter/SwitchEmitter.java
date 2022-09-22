/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a switch block where inner blocks are only rendered if their name matches the switch expression.
 *
 * @see sirius.pasta.tagliatelle.tags.SwitchTag
 */
public class SwitchEmitter extends Emitter {

    protected Map<String, Emitter> blocks;
    protected Callable switchExpression;

    /**
     * Creates a new emitter for the given position.
     *
     * @param startOfBlock the position where the switch block was defined.
     */
    public SwitchEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    public void setBlocks(Map<String, Emitter> blocks) {
        this.blocks = blocks;
    }

    public void setSwitchExpression(Callable switchExpression) {
        this.switchExpression = switchExpression;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        if (switchExpression == null) {
            return;
        }

        String block = String.valueOf(switchExpression.call(context));
        if (Strings.isFilled(block)) {
            blocks.getOrDefault(block, ConstantEmitter.EMPTY).emitToContext(context);
        }
    }

    /**
     * Reduces the switch expression as well as the inner blocks.
     * <p>
     * If the switch expression becomes constant, the emitter is reduced the the respective blocks to render.
     *
     * @return either the emitter itself or, if the expression is constant, the inner blocks
     */
    @Override
    public Emitter reduce() {
        if (switchExpression instanceof ConstantCall) {
            try {
                Object value = switchExpression.call(null);
                if (Strings.isEmpty(value) || !blocks.containsKey(value)) {
                    return ConstantEmitter.EMPTY;
                }

                return blocks.get(value).reduce();
            } catch (ScriptingException e) {
                throw Exceptions.createHandled().withDirectMessage(e.getMessage()).handle();
            }
        }

        if (blocks != null) {
            Map<String, Emitter> copy = new HashMap<>();
            for (Map.Entry<String, Emitter> e : blocks.entrySet()) {
                copy.put(e.getKey(), e.getValue().reduce());
            }
            this.blocks = copy;
        }

        return this;
    }

    @Override
    public String toString() {
        return "switch on " + switchExpression;
    }
}
