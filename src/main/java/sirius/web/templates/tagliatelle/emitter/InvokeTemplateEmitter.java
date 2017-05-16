/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.emitter;

import sirius.kernel.health.Exceptions;
import sirius.web.templates.tagliatelle.CompileException;
import sirius.web.templates.tagliatelle.LocalRenderContext;
import sirius.web.templates.tagliatelle.Template;
import sirius.web.templates.tagliatelle.TemplateArgument;
import sirius.web.templates.tagliatelle.expression.Expression;

import java.util.Map;

/**
 * Created by aha on 16.05.17.
 */
public class InvokeTemplateEmitter implements Emitter {

    private static final Expression[] NO_ARGS = {};
    private String templateName;
    private Expression[] arguments = NO_ARGS;
    private Map<String, Emitter> blocks = null;

    public InvokeTemplateEmitter(String templateName) {
        this.templateName = templateName;
    }

    @Override
    public void emit(LocalRenderContext context) {
        try {
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
        } catch (CompileException e) {
            Exceptions.handle(e);
        }
    }

    public void setArguments(Expression[] args) {
        this.arguments = args;
    }

    public void setBlocks(Map<String, Emitter> blocks) {
        this.blocks = blocks;
    }
}
