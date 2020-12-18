/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.tagliatelle.Template;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;
import sirius.pasta.tagliatelle.rendering.RenderException;

import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;

/**
 * Invokes and renders a sub template at runtime.
 */
public class DynamicInvokeTemplateEmitter extends Emitter {

    private Map<String, Callable> args;
    private Callable templateName;
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
                                        Callable templateName,
                                        Map<String, Callable> args,
                                        Map<String, Emitter> blocks) {
        super(startOfBlock);
        this.templateName = templateName;
        this.args = args;
        this.blocks = blocks;
    }

    @Override
    public Emitter reduce() {
        if (blocks != null) {
            Map<String, Emitter> copyBlocks = new HashMap<>();
            blocks.forEach((key, value) -> copyBlocks.put(key, value.reduce()));
            this.blocks = copyBlocks;
        }

        return this;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        String effectiveTemplateName = String.valueOf(templateName.call(context));
        Template template = context.resolve(effectiveTemplateName)
                                   .orElseThrow(() -> new FileNotFoundException(effectiveTemplateName));

        LocalRenderContext subContext = context.createChildContext(template);
        subContext.setBlocks(context, blocks);
        try {
            Map<String, Object> effectiveArgs = new HashMap<>();
            for (Map.Entry<String, Callable> entry : args.entrySet()) {
                effectiveArgs.put(entry.getKey(), entry.getValue().call(context));
            }
            template.transferArguments(effectiveArgs, subContext);
            emitDebugMessage(context, "start rendering dynamic template '%s'", effectiveTemplateName);
            template.renderWithContext(subContext);
            emitDebugMessage(context, "finish rendering dynamic template '%s'", effectiveTemplateName);
        } catch (ScriptingException ex) {
            throw RenderException.create(context, ex);
        } finally {
            subContext.release();
        }
    }
}
