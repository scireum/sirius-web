/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.tagliatelle.Template;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;

/**
 * Invokes and renders a sub template at runtime.
 */
public class InvokeTemplateEmitter extends Emitter {

    private static final Callable[] NO_ARGS = {};
    private String templateName;
    private Callable[] arguments = NO_ARGS;
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
    public Emitter reduce() {
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
    protected void emitToContext(LocalRenderContext context) throws Exception {
        Template template = context.resolve(templateName).orElseThrow(() -> new FileNotFoundException(templateName));
        LocalRenderContext subContext = context.createChildContext(template);
        if (blocks != null) {
            blocks.forEach((k, v) -> emitDebugMessage(context,
                                                      "defining block '%s' from template '%s'",
                                                      k,
                                                      context.getTemplate().getName()));
        }
        subContext.setBlocks(context, blocks);

        try {
            int index = 0;
            for (TemplateArgument arg : template.getArguments()) {
                applyArgument(context, template, subContext, index, arg);
                index++;
            }

            emitDebugMessage(context, "start rendering template '%s'", templateName);
            template.renderWithContext(subContext);
            emitDebugMessage(context, "finish rendering template '%s'", templateName);
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
        Object argumentValue = determineArgumentValue(context, template, subContext, index, arg);

        if (!CompilationContext.isAssignable(argumentValue, arg.getType())) {
            throw new IllegalArgumentException(Strings.apply(
                    "An invalid argument was provided for '%s' when calling '%s'. Given: %s but expected was: %s",
                    arg.getName(),
                    template,
                    argumentValue == null ? null : argumentValue.getClass(),
                    arg.getType()));
        }

        subContext.writeVariable(index, argumentValue);
    }

    private Object determineArgumentValue(LocalRenderContext context,
                                          Template template,
                                          LocalRenderContext subContext,
                                          int index,
                                          TemplateArgument arg) {
        try {
            if (index < arguments.length && arguments[index] != null) {
                return arguments[index].call(context);
            }

            if (arg.getDefaultValue() != null) {
                return arg.getDefaultValue().call(subContext);
            } else {
                throw new IllegalArgumentException(Strings.apply(
                        "Neither a value nor a default value was provided for argument '%s' when calling '%s'",
                        arg.getName(),
                        template));
            }
        } catch (ScriptingException e) {
            throw new IllegalArgumentException(Strings.apply("Cannot compute effective argument value: %s",
                                                             e.getMessage()));
        }
    }

    /**
     * Sets the argument expressions for the template invocation.
     * <p>
     * Note that the order must match the arguments of the template itself.
     *
     * @param args the expressions to evaluate and supply as arguments
     */
    public void setArguments(Callable[] args) {
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
