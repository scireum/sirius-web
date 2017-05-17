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
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Template template = context.resolve(templateName);
        LocalRenderContext subContext = context.createChildContext(template);
        subContext.setBlocks(blocks);

        try {
            int index = 0;
            for (TemplateArgument arg : template.getArguments()) {
                Object argumentValue = null;
                if (index < arguments.length) {
                    argumentValue = arguments[index].eval(context);
                } else {
                    if (arg.getDefaultValue() == null) {
                        //TODO warn / fail!
                        argumentValue = arg.getDefaultValue().eval(subContext);
                    }
                }

                if (!arg.getType().isAssignableFrom(argumentValue.getClass())) {
                    //TODO warn / fail
                }

                subContext.setLocal(index, argumentValue);
                index++;
            }

            template.render(subContext);
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
