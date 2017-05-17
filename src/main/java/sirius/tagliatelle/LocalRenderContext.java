/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import parsii.tokenizer.Position;
import sirius.tagliatelle.emitter.Emitter;

import java.util.Map;

/**
 * Created by aha on 10.05.17.
 */
public class LocalRenderContext {

    private Template template;
    private RenderStack.View locals;
    private Position position = Position.UNKNOWN;
    private GlobalRenderContext globalContext;
    private LocalRenderContext parent;
    private Map<String, Emitter> blocks;

    protected LocalRenderContext(Template template, GlobalRenderContext globalContext, RenderStack.View locals) {
        this.globalContext = globalContext;
        this.template = template;
        this.locals = locals;
    }

    public LocalRenderContext createChildContext(Template template) {
        LocalRenderContext ctx = globalContext.createContext(template);
        ctx.parent = this;

        return ctx;
    }

    public LocalRenderContext createClosureContext(LocalRenderContext enclosedContext) {
        LocalRenderContext ctx =
                new LocalRenderContext(enclosedContext.template, globalContext, enclosedContext.locals);
        ctx.parent = this;
        ctx.blocks = enclosedContext.blocks;

        return ctx;
    }

    public void release() {
        globalContext.release(this);
    }

    public void output(String content) {
        globalContext.stringConsumer.accept(content);
    }

    public boolean isAcceptingBytes() {
        return globalContext.byteConsumer != null;
    }

    public void output(byte[] contentAsBytes) {
        globalContext.byteConsumer.accept(contentAsBytes);
    }


    public void setLocal(int index, Object variable) {
        locals.writeLocal(index, variable);
    }

    public Object getGlobal(int index) {
        return globalContext.globals.get(index);
    }

    public Object getLocal(int index) {
        return locals.readLocal(index);
    }

    public RenderStack.View getLocals() {
        return locals;
    }

    public Template resolve(String templateName) throws CompileException {
        return globalContext.resolve(templateName);
    }

    public void emitBlock(String name) throws RenderException {
        if (blocks == null) {
            return;
        }
        Emitter emitter = blocks.get(name);
        if (emitter == null) {
            return;
        }

        LocalRenderContext subContext = createClosureContext(parent);
        emitter.emit(subContext);
    }

    public void setBlocks(Map<String, Emitter> blocks) {
        this.blocks = blocks;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        LocalRenderContext ctx = this;
        while (ctx != null) {
            sb.append(String.format("%3d:%2d: %s\n", ctx.position.getLine(), ctx.position.getPos(), ctx.template));
            ctx = ctx.parent;
        }
        return sb.toString();
    }

    public void updatePosition(Position newPosition) {
        this.position = newPosition;
    }
}
