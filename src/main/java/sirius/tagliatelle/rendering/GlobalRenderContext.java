/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.rendering;

import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.compiler.CompileException;
import sirius.web.templates.ContentHelper;

import javax.annotation.Nonnull;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * Represents the global context which is created to render a template.
 *
 * @see Tagliatelle#createRenderContext()
 */
public class GlobalRenderContext {

    private List<Object> globals;
    protected Map<String, Template> templateCache;
    protected StackAllocator stack = new StackAllocator();
    protected Tagliatelle engine;
    protected StringBuilder buffer;
    protected Map<String, String> extraBlocks;
    protected Function<String, String> escaper = GlobalRenderContext::escapeRAW;

    /**
     * Creates a new render context.
     * <p>
     * Use {@link Tagliatelle#createRenderContext()} to obtain an instance.
     *
     * @param engine the global engine instance.
     */
    public GlobalRenderContext(Tagliatelle engine) {
        this.engine = engine;
        this.buffer = new StringBuilder();
    }

    /**
     * Resolves the given template.
     * <p>
     * A local cache is maintained so that a previously resolved template is directly re-used and not resolved several
     * times.
     *
     * @param templateName the name of the template to resolve
     * @return the resolved template wrapped as optional or an empty optional, if no such template exists
     * @throws CompileException in case the resolved template has compile errors
     */
    protected Optional<Template> resolve(String templateName) throws CompileException {
        if (templateCache != null) {
            Template result = templateCache.get(templateName);
            if (result != null) {
                return Optional.of(result);
            }
        }

        Optional<Template> result = engine.resolve(templateName);
        if (!result.isPresent()) {
            return result;
        }

        if (templateCache == null) {
            templateCache = new HashMap<>();
        }

        templateCache.put(templateName, result.get());

        return result;
    }

    /**
     * Adds unescaped output to the result buffer.
     *
     * @param string the string to output
     * @see LocalRenderContext#outputRaw(String)
     */
    protected void outputRaw(String string) {
        if (string != null) {
            buffer.append(string);
        }
    }

    /**
     * Adds escaped output to the result buffer.
     * <p>
     * Utilizes the current {@link #escaper} to escape the given string.
     *
     * @param string the string to output
     * @see LocalRenderContext#outputEscaped(String) (String)
     */
    protected void outputEscaped(String string) {
        if (string != null) {
            buffer.append(escaper.apply(string));
        }
    }

    /**
     * Specifies the escaper to use.
     *
     * @param escaper a mapping function to escape certain characters in strings being output using {@link
     *                #outputEscaped(String)}.
     */
    public void setEscaper(@Nonnull Function<String, String> escaper) {
        this.escaper = escaper;
    }

    /**
     * Returns the currently active escaper.
     *
     * @return the currently active escaper
     */
    public Function<String, String> getEscaper() {
        return escaper;
    }

    /**
     * Provides a raw escaper which doesn't alter the input at all.
     *
     * @param input the string to output
     * @return the given string
     */
    public static String escapeRAW(String input) {
        return input;
    }

    /**
     * Provides a XML escaper which processes XML control characters.
     *
     * @param input the string to output
     * @return the given string where XML control characters were replaced using {@link ContentHelper#escapeXML(Object)}
     */
    public static String escapeXML(String input) {
        return ContentHelper.escapeXML(input);
    }

    /**
     * Creates a local render context to invoke the given template.
     *
     * @param template the template to invoke
     * @return a local context with an appropriate (empty) local stack
     */
    public LocalRenderContext createContext(Template template) {
        return new LocalRenderContext(template, this, stack.alloc(template.getStackDepth()));
    }

    /**
     * Releases the local render context to free up the allocated stack.
     * <p>
     * Note that {@link #createContext(Template)} and {@link #release(LocalRenderContext)} must be folded appropriately.
     *
     * @param renderContext the context to free up
     */
    protected void release(LocalRenderContext renderContext) {
        stack.free(renderContext.getLocals());
    }

    @Override
    public String toString() {
        return buffer.toString();
    }

    /**
     * Provides access to the environment (global variables).
     *
     * @return the list of global variables
     */
    public List<Object> getGlobals() {
        if (globals == null) {
            this.globals = engine.createEnvironment();
        }
        return globals;
    }

    /**
     * Returns the contents for the given extra block, which were create during rendering.
     * <p>
     * Using &lt;i:block&gt; tags at the top level permits to output additional rendering results, next to the main
     * string.
     *
     * @param name the name of the block
     * @return the result generated while rendering the template or an empty string if no content is available.
     */
    @Nonnull
    public String getExtraBlock(String name) {
        if (extraBlocks == null) {
            return "";
        }

        String result = extraBlocks.getOrDefault(name, null);
        return result != null ? result : "";
    }

    /**
     * Emits everything which is invoked from within the callback into an unescaped string.
     *
     * @param callback     the callback which will invoke emitters.
     * @param escaperToUse the effective escaper to use. As the final string will most probably be emitted again,
     *                     {@link #escapeRAW(String)} is most probably the one to use.
     * @return the contents which were emitted within the <tt>callback</tt>
     */
    public String emitToString(RenderCall callback, Function<String, String> escaperToUse) {
        StringBuilder backupBuffer = this.buffer;
        Function<String, String> backupEscaper = this.escaper;
        this.buffer = new StringBuilder();
        this.escaper = escaperToUse;

        try {
            callback.render();
            return buffer.toString().trim();
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        } finally {
            this.escaper = backupEscaper;
            this.buffer = backupBuffer;
        }
    }

    /**
     * Stores the rendering of an extra block.
     *
     * @param name     the name of the block to store
     * @param contents the contents to store
     */
    public void storeExtraBlock(String name, String contents) {
        if (extraBlocks == null) {
            extraBlocks = new HashMap<>();
        }

        extraBlocks.put(name, contents);
    }
}
