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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    private static final Pattern OPENING_SCRIPT_TAG = Pattern.compile("<script(\\s.*)?>", Pattern.CASE_INSENSITIVE);
    private static final Pattern CLOSING_SCRIPT_TAG = Pattern.compile("</script>", Pattern.CASE_INSENSITIVE);
    private static final Pattern OPENING_STYLE_TAG = Pattern.compile("<style(\\s.*)?>", Pattern.CASE_INSENSITIVE);
    private static final Pattern CLOSING_STYLE_TAG = Pattern.compile("</style>", Pattern.CASE_INSENSITIVE);

    /**
     * The name of the Cookie that enables debugging of rendered contents.
     */
    public static final String SIRIUS_DEBUG_COOKIE = "SIRIUS.WEB.DEBUG.LEVEL";

    /**
     * Contains different levels that represent debug message prevalence when rendering contents.
     */
    public enum DebugLevel {
        OFF, DEBUG, TRACE
    }

    /**
     * Stores the current debug level.
     */
    protected DebugLevel debugLevel = DebugLevel.OFF;

    /**
     * Number of open <script> tags in the current buffer.
     */
    protected int openScripts = 0;

    /**
     * Number of open <style> tags in the current buffer.
     */
    protected int openStyles = 0;

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

            if (debugLevel != DebugLevel.OFF) {
                Matcher matcher = OPENING_SCRIPT_TAG.matcher(string);
                while (matcher.find()) {
                    openScripts++;
                }
                matcher = CLOSING_SCRIPT_TAG.matcher(string);
                while (matcher.find()) {
                    openScripts--;
                }
                matcher = OPENING_STYLE_TAG.matcher(string);
                while (matcher.find()) {
                    openStyles++;
                }
                matcher = CLOSING_STYLE_TAG.matcher(string);
                while (matcher.find()) {
                    openStyles--;
                }
            }
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
            outputRaw(escaper.apply(string));
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
     * @param callback the callback which will invoke emitters.
     * @return the contents which were emitted within the <tt>callback</tt>
     */
    public String emitToString(RenderCall callback) {
        StringBuilder backupBuffer = this.buffer;
        this.buffer = new StringBuilder();

        try {
            callback.render();
            return buffer.toString().trim();
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        } finally {
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

    /**
     * Returns the current debug level for rendered contents.
     */
    public DebugLevel getSiriusDebugLevel() {
        return this.debugLevel;
    }

    /**
     * Sets the debug level for rendered contents.
     *
     * @param debugLevel {@link DebugLevel} to set
     */
    public void setSiriusDebugLevel(@Nonnull DebugLevel debugLevel) {
        this.debugLevel = debugLevel;
    }

    /**
     * Checks if the provided debug level should be emitted based on the current debug level.
     *
     * @param levelToCompare log level for the message which wants to be emitted
     * @return true when desired log level higher or equal to current log level
     */
    public boolean canEmitDebug(@Nonnull DebugLevel levelToCompare) {
        return debugLevel.ordinal() >= levelToCompare.ordinal();
    }

    /**
     * Prints comments properly escaping contents according to the current output being printed.
     *
     * @param string message to output as comments
     */
    public void outputDebug(String string) {
        if (string != null) {
            if (openStyles > 0 || openScripts > 0) {
                buffer.append("\n/* " + string + " */\n");
            } else {
                buffer.append("<!-- " + string + " -->");
            }
        }
    }
}
