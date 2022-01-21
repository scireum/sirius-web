/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.rendering;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Explain;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.tagliatelle.Template;
import sirius.pasta.tagliatelle.emitter.ConstantEmitter;
import sirius.pasta.tagliatelle.emitter.Emitter;

import java.io.IOException;
import java.util.Map;
import java.util.Optional;

/**
 * Represents a render context local to a single template.
 * <p>
 * If one template invokes another, they share a common {@link GlobalRenderContext} but have their own {@link
 * LocalRenderContext}.
 * <p>
 * Also a local context knows its parent and can therefore output the <tt>render stack</tt> which is kind of a
 * stacktrace for templates.
 */
public class LocalRenderContext implements Environment {

    private final Template template;
    private final StackAllocator.View locals;
    private Position position = Position.UNKNOWN;
    private final GlobalRenderContext globalContext;
    private LocalRenderContext parent;
    private Map<String, Emitter> blocks;
    private LocalRenderContext enclosedContext;

    /**
     * Creates a new context.
     * <p>
     * Use one of the helper methods to create a purpose built context.
     *
     * @param template      the template for which the context was created.
     * @param globalContext the global and shared context for the render process
     * @param locals        the local / stack variables. These can also be shared, if a closure (block included in
     *                      another template) is created.
     */
    protected LocalRenderContext(Template template, GlobalRenderContext globalContext, StackAllocator.View locals) {
        this.globalContext = globalContext;
        this.template = template;
        this.locals = locals;
    }

    /**
     * Creates a child context to invoke the given template.
     *
     * @param template the template which will be invoked
     * @return a newly created child context for the given template
     */
    public LocalRenderContext createChildContext(Template template) {
        LocalRenderContext ctx = globalContext.createContext(template);
        ctx.parent = this;

        return ctx;
    }

    /**
     * Creates a closure context which can be put on the render stack as child, to provide access to the variables
     * of the enclosed context and to maintain a valid renderstack.
     *
     * @return a new closure context which can be used as child of this context but references the template an locals of
     * the enclosed context
     */
    public LocalRenderContext createClosureContext() {
        LocalRenderContext ctx =
                new LocalRenderContext(enclosedContext.template, globalContext, enclosedContext.locals);
        ctx.parent = parent;
        ctx.blocks = enclosedContext.blocks;
        ctx.enclosedContext = enclosedContext.enclosedContext;

        return ctx;
    }

    /**
     * Releases this context.
     */
    public void release() {
        globalContext.release(this);
    }

    /**
     * Adds unescaped output to the result buffer.
     *
     * @param content the string to output
     * @throws IOException in case of an IO error
     * @see LocalRenderContext#outputRaw(String)
     */
    @SuppressWarnings("squid:RedundantThrowsDeclarationCheck")
    @Explain("Sublclasses might throw an IOException here.")
    public void outputRaw(String content) throws IOException {
        globalContext.outputRaw(content);
    }

    /**
     * Adds escaped output to the result buffer.
     * <p>
     * Utilizes the current {@link GlobalRenderContext#escaper} to escape the given string.
     *
     * @param content the string to output
     * @throws IOException in case of an IO error
     * @see LocalRenderContext#outputEscaped(String) (String)
     */
    @SuppressWarnings("squid:RedundantThrowsDeclarationCheck")
    @Explain("Sublclasses might throw an IOException here.")
    public void outputEscaped(String content) throws IOException {
        globalContext.outputEscaped(content);
    }

    /**
     * Adds commented output to the result buffer.
     *
     * @param content the string to output
     * @throws IOException in case of an IO error
     * @see GlobalRenderContext#outputDebug(String)
     */
    @SuppressWarnings("squid:RedundantThrowsDeclarationCheck")
    @Explain("Sublclasses might throw an IOException here.")
    public void outputDebug(String content) throws IOException {
        globalContext.outputDebug(content);
    }

    @Override
    public void writeVariable(int index, Object value) {
        locals.writeLocal(index, value);
    }

    @Override
    public Object readVariable(int index) {
        return locals.readLocal(index);
    }

    /**
     * Contains the locals used by this context.
     *
     * @return the block of locals used by this context
     */
    protected StackAllocator.View getLocals() {
        return locals;
    }

    /**
     * Resolves the given template.
     * <p>
     * A local cache is maintained by the {@link GlobalRenderContext} so that a previously resolved template is directly
     * re-used and not resolved several times.
     *
     * @param templateName the name of the template to resolve
     * @return the resolved template wrapped as optional or an empty optional, if no such template exists
     * @throws CompileException in case the resolved template has compile errors
     */
    public Optional<Template> resolve(String templateName) throws CompileException {
        return globalContext.resolve(templateName);
    }

    /**
     * Emits the block with the given name.
     *
     * @param name the name of the block to emit
     * @return <tt>true</tt> if a block was found and emitted, <tt>false</tt> otherwise
     * @throws RenderException in case of an error when emitting the block
     */
    public boolean emitBlock(String name) throws RenderException {
        if (!blockExists(name)) {
            return false;
        }
        Emitter emitter = blocks.get(name);

        if (emitter instanceof ConstantEmitter) {
            emitter.emit(this);
        } else {
            LocalRenderContext subContext = createClosureContext();
            emitter.emit(subContext);
        }

        return true;
    }

    /**
     * Specifies the blocks made available by the caller.
     *
     * @param context the context being enclosed along with the blocks to be restored later to access local variables
     * @param blocks  the blocks passed to the template being rendered
     */
    public void setBlocks(LocalRenderContext context, Map<String, Emitter> blocks) {
        this.enclosedContext = context;
        this.blocks = blocks;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        LocalRenderContext ctx = this;
        while (ctx != null) {
            sb.append(String.format("%3d:%2d: %s%n", ctx.position.getLine(), ctx.position.getPos(), ctx.template));
            ctx = ctx.parent;
        }

        return sb.toString();
    }

    /**
     * Updates the output position.
     *
     * @param newPosition the position within the template being rendered.
     */
    public void updatePosition(Position newPosition) {
        this.position = newPosition;
    }

    /**
     * Provides access to the underlying global context.
     *
     * @return the global context
     */
    public GlobalRenderContext getGlobalContext() {
        return globalContext;
    }

    /**
     * Checks if the given block exists.
     *
     * @param name name of the block to check for existance
     * @return true if block found otherwise false
     */
    public boolean blockExists(String name) {
        if (blocks == null) {
            return false;
        }
        return blocks.containsKey(name);
    }

    /**
     * Provides access to the underlying template.
     *
     * @return the {@link Template}
     */
    public Template getTemplate() {
        return this.template;
    }

    /**
     * Returns the root context for which a rendering process has been started.
     * @return
     */
    public LocalRenderContext getRootContext() {
        if (this.parent == null) {
            return this;
        } else {
            return parent.getRootContext();
        }
    }
}
