/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.LocalRenderContext;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ExpressionVisitor;

import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by aha on 16.05.17.
 */
public class InvokeTemplateEmitter extends Emitter {

    private static final Expression[] NO_ARGS = {};
    private String templateName;
    private Expression[] arguments = NO_ARGS;
    private Map<String, Emitter> blocks = null;

    public InvokeTemplateEmitter(Position startOfBlock, String templateName) {
        super(startOfBlock);
        this.templateName = templateName;
    }

    @Override
    public Emitter copy() {
        InvokeTemplateEmitter copy = new InvokeTemplateEmitter(startOfBlock, templateName);
        copy.arguments = new Expression[arguments.length];
        for (int i = 0; i < arguments.length; i++) {
            copy.arguments[i] = arguments[i].copy();
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
            arguments[i] = arguments[i].reduce();
        }

        if (blocks != null) {
            Map<String, Emitter> copy = new HashMap<>();
            for (Map.Entry<String, Emitter> e : blocks.entrySet()) {
                copy.put(e.getKey(), e.getValue().copy());
            }
            this.blocks = copy;
        }

        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(ExpressionVisitor visitor) {
        for (int i = 0; i < arguments.length; i++) {
            arguments[i] = visitor.visit(arguments[i]);
        }

        if (blocks != null) {
            this.blocks.values().forEach(e -> e.visitExpressions(visitor));
        }
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Template template = context.resolve(templateName).orElseThrow(() -> new FileNotFoundException(templateName));
        LocalRenderContext subContext = context.createChildContext(template);
        subContext.setBlocks(blocks);

        try {
            int index = 0;
            for (TemplateArgument arg : template.getArguments()) {
                Object argumentValue = null;
                if (index < arguments.length && arguments[index] != null) {
                    argumentValue = arguments[index].eval(context);
                } else {
                    if (arg.getDefaultValue() != null) {
                        argumentValue = arg.getDefaultValue().eval(subContext);
                    } else {
                        //TODO warn / fail!

                    }
                }

                if (!arg.getType().isAssignableFrom(argumentValue.getClass())) {
                    //TODO warn / fail
                }

                subContext.setLocal(index, argumentValue);
                index++;
            }

            template.renderWithContext(subContext);
        } finally {
            subContext.release();
        }
    }

    public void setArguments(Expression[] args) {
        this.arguments = args;
    }

    public void setBlocks(Map<String, Emitter> blocks) {
        this.blocks = blocks;
    }
}
