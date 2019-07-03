/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

/**
 * Invokes and renders a sub template at runtime.
 */
public class InvokeTemplateEmitter extends Emitter {

    private static final Expression[] NO_ARGS = {};
    private String templateName;
    private Expression[] arguments = NO_ARGS;
    private Map<String, Emitter> blocks = null;

    /**
     * Creates a new instance at the given position with the given target template.
     *
     * @param startOfBlock the position where the invocation was declared
     * @param templateName the name of the template to invoke
     */
    public InvokeTemplateEmitter(Position startOfBlock, String templateName) {
        super(startOfBlock);
        this.templateName = templateName;
    }

    @Override
    public Emitter copy() {
        InvokeTemplateEmitter copy = new InvokeTemplateEmitter(startOfBlock, templateName);
        if (arguments != NO_ARGS) {
            copy.arguments = new Expression[arguments.length];
            for (int i = 0; i < arguments.length; i++) {
                Expression arg = arguments[i];
                copy.arguments[i] = arg != null ? arg.copy() : null;
            }
        }

        if (blocks != null) {
            copy.blocks = new HashMap<>();
            for (Map.Entry<String, Emitter> e : blocks.entrySet()) {
                copy.blocks.put(e.getKey(), e.getValue().copy());
            }
        }

        return copy;
    }

    @Override
    public Emitter reduce() {
        for (int i = 0; i < arguments.length; i++) {
            Expression arg = arguments[i];
            arguments[i] = arg != null ? arg.reduce() : null;
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
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        ExpressionVisitor visitor = visitorSupplier.apply(getStartOfBlock());
        for (int i = 0; i < arguments.length; i++) {
            if (arguments[i] != null) {
                arguments[i] = arguments[i].propagateVisitor(visitor);
            }
        }

        if (blocks != null) {
            this.blocks.values().forEach(e -> e.visitExpressions(visitorSupplier));
        }
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Template template = context.resolve(templateName).orElseThrow(() -> new FileNotFoundException(templateName));
        LocalRenderContext subContext = context.createChildContext(template);
        subContext.setBlocks(context, blocks);

        try {
            int index = 0;
            for (TemplateArgument arg : template.getArguments()) {
                applyArgument(context, template, subContext, index, arg);
                index++;
            }

            template.renderWithContext(subContext);
        } finally {
            subContext.release();
        }
    }

    /**
     * Evaluates the expression or its default expression and transfers the value to the given sub context.
     *
     * @param context    the context of the caller
     * @param template   the template being called
     * @param subContext the context of the callee
     * @param index      the index of the parameter being transferred
     * @param arg        the template argument being applied
     */
    private void applyArgument(LocalRenderContext context,
                               Template template,
                               LocalRenderContext subContext,
                               int index,
                               TemplateArgument arg) {
        Object argumentValue;
        if (index < arguments.length && arguments[index] != null) {
            argumentValue = arguments[index].eval(context);
        } else {
            if (arg.getDefaultValue() != null) {
                argumentValue = arg.getDefaultValue().eval(subContext);
            } else {
                throw new IllegalArgumentException(Strings.apply(
                        "Neither a value nor a default value was provided for argument '%s' when calling '%s'",
                        arg.getName(),
                        template));
            }
        }

        if (!Tagliatelle.isAssignable(argumentValue, arg.getType())) {
            throw new IllegalArgumentException(Strings.apply(
                    "An invalid argument was provided for '%s' when calling '%s'. Given: %s but expected was: %s",
                    arg.getName(),
                    template,
                    argumentValue == null ? null : argumentValue.getClass(),
                    arg.getType()));
        }

        subContext.setLocal(index, argumentValue);
    }

    /**
     * Sets the argument expressions for the template invocation.
     * <p>
     * Note that the order must match the arguments of the template itself.
     *
     * @param args the expressions to evaluate and supply as arguments
     */
    public void setArguments(Expression[] args) {
        this.arguments = args;
    }

    /**
     * Sets the blocks to be applied, which can be referenced by the invoked template.
     *
     * @param blocks the blocks passed to the templates
     */
    public void setBlocks(Map<String, Emitter> blocks) {
        this.blocks = blocks;
    }
}
