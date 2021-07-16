/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;

/**
 * Handles attributes which are only emitted if the value evaluates to neither <tt>null</tt> nor <tt>false</tt>.
 * <p>
 * Therefore an expression like {@code @attr="something"} is either output as <tt>attr="attr"</tt> if "something"
 * evaluates to <tt>true</tt> or <tt>att="value"</tt> if "something" evaluates to the value.
 */
public class AttributeExpressionEmitter extends Emitter {

    private final String attibuteName;
    private final Callable attributeExpression;

    /**
     * Contains a new emitter with the given position.
     *
     * @param startOfBlock        the start position where the emitter was created
     * @param attibuteName        the name of the attribute to emit
     * @param attributeExpression the expression to evaluate and to output the result
     */
    public AttributeExpressionEmitter(@Nonnull Position startOfBlock,
                                      String attibuteName,
                                      Callable attributeExpression) {
        super(startOfBlock);
        this.attibuteName = attibuteName;
        this.attributeExpression = attributeExpression;
    }

    @Override
    @SuppressWarnings("java:S2589")
    @Explain("value is actually a tri-state here - it can be true/false to control the attribute "
             + "OR it can contain a value to output if it is neither of both.")
    protected void emitToContext(@Nonnull LocalRenderContext context) throws Exception {
        Object value = attributeExpression.call(context);
        if (Boolean.TRUE.equals(value)) {
            context.outputRaw(attibuteName);
            context.outputRaw("=\"");
            context.outputRaw(attibuteName);
            context.outputRaw("\"");
        } else if (Strings.isFilled(value) && !Boolean.FALSE.equals(value)) {
            context.outputRaw(attibuteName);
            context.outputRaw("=\"");
            context.outputRaw(NLS.toMachineString(value));
            context.outputRaw("\"");
        }
    }

    @Nonnull
    @Override
    @SuppressWarnings("java:S2589")
    @Explain("value is actually a tri-state here - it can be true/false to control the attribute "
             + "OR it can contain a value to output if it is neither of both.")
    public Emitter reduce() {
        if (attributeExpression instanceof ConstantCall) {
            try {
                Object value = attributeExpression.call(null);
                if (Boolean.TRUE.equals(value)) {
                    return new ConstantEmitter(startOfBlock).append(attibuteName)
                                                            .append("=\"")
                                                            .append(attibuteName)
                                                            .append("\"");
                } else if (Strings.isEmpty(value) || Boolean.FALSE.equals(value)) {
                    return ConstantEmitter.EMPTY;
                } else {
                    return new ConstantEmitter(startOfBlock).append(attibuteName)
                                                            .append("=\"")
                                                            .append(NLS.toMachineString(value))
                                                            .append("\"");
                }
            } catch (ScriptingException e) {
                Exceptions.ignore(e);
            }
        }

        return this;
    }
}
