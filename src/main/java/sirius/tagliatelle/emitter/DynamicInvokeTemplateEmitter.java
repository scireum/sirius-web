/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.Template;
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
public class DynamicInvokeTemplateEmitter extends Emitter {
    private static final String USE_CUSTOMIZATION = "useCustomization";

    private Map<String, Expression> args;
    private Expression templateName;
    private Map<String, Emitter> blocks = null;

    /**
     * Creates a new instance at the given position with the given target template.
     *
     * @param startOfBlock the position where the invocation was declared
     * @param templateName the expression which defines the template to invoke
     * @param args         the arguments passed to the template
     * @param blocks       the blocks passed to the template
     */
    public DynamicInvokeTemplateEmitter(Position startOfBlock,
                                        Expression templateName,
                                        Map<String, Expression> args,
                                        Map<String, Emitter> blocks) {
        super(startOfBlock);
        this.templateName = templateName;
        this.args = args;
        this.blocks = blocks;
    }

    @Override
    public Emitter copy() {
        Map<String, Expression> copyArgs = new HashMap<>();
        Map<String, Emitter> copyBlocks = new HashMap<>();
        args.forEach((key, value) -> copyArgs.put(key, value.copy()));
        if (blocks != null) {
            blocks.forEach((key, value) -> copyBlocks.put(key, value.copy()));
        }

        return new DynamicInvokeTemplateEmitter(startOfBlock, templateName.copy(), copyArgs, copyBlocks);
    }

    @Override
    public Emitter reduce() {
        this.templateName = templateName.reduce();
        Map<String, Expression> copyArgs = new HashMap<>();
        args.forEach((key, value) -> copyArgs.put(key, value.reduce()));
        this.args = copyArgs;

        if (blocks != null) {
            Map<String, Emitter> copyBlocks = new HashMap<>();
            blocks.forEach((key, value) -> copyBlocks.put(key, value.reduce()));
            this.blocks = copyBlocks;
        }

        return this;
    }

    @Override
    public Emitter propagateVisitor(EmitterVisitor visitor) {
        if (blocks != null) {
            Map<String, Emitter> copy = new HashMap<>();
            for (Map.Entry<String, Emitter> e : blocks.entrySet()) {
                copy.put(e.getKey(), e.getValue().propagateVisitor(visitor));
            }
            this.blocks = copy;
        }
        return visitor.visitThis(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        ExpressionVisitor visitor = visitorSupplier.apply(getStartOfBlock());
        templateName = templateName.propagateVisitor(visitor);

        Map<String, Expression> copyArgs = new HashMap<>();
        args.forEach((key, value) -> copyArgs.put(key, value.propagateVisitor(visitor)));
        this.args = copyArgs;

        if (blocks != null) {
            this.blocks.values().forEach(block -> block.visitExpressions(visitorSupplier));
        }
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        String effectiveTemplateName = String.valueOf(templateName.eval(context));
        Template template = context.resolve(effectiveTemplateName, useCustomization(context))
                .orElseThrow(() -> new FileNotFoundException(effectiveTemplateName));

        LocalRenderContext subContext = context.createChildContext(template);
        subContext.setBlocks(context, blocks);
        try {
            Map<String, Object> effectiveArgs = new HashMap<>();
            args.forEach((k, e) -> effectiveArgs.put(k, e.eval(context)));
            template.transferArguments(effectiveArgs, subContext);
            template.renderWithContext(subContext);
        } finally {
            subContext.release();
        }
    }

    private boolean useCustomization(LocalRenderContext context) {
        return !args.containsKey(USE_CUSTOMIZATION) || (Boolean) args.get(USE_CUSTOMIZATION).eval(context);
    }
}
