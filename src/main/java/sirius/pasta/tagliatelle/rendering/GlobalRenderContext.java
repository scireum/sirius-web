/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.rendering;

import sirius.kernel.commons.StringCleanup;
import sirius.kernel.commons.Strings;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.tagliatelle.Tagliatelle;
import sirius.pasta.tagliatelle.Template;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Represents the global context which is created to render a template.
 *
 * @see Tagliatelle#createRenderContext()
 */
public class GlobalRenderContext {

    protected Map<String, Template> templateCache;
    protected StackAllocator stack = new StackAllocator();
    protected Tagliatelle engine;
    protected StringBuilder buffer;
    protected Map<String, String> extraBlocks;
    protected UnaryOperator<String> escaper = GlobalRenderContext::escapeRAW;

    private Set<String> guards = new HashSet<>();

    private static final Pattern OPENING_SCRIPT_TAG = Pattern.compile("<script(\\s.*)?>", Pattern.CASE_INSENSITIVE);
    private static final Pattern CLOSING_SCRIPT_TAG = Pattern.compile("</script>", Pattern.CASE_INSENSITIVE);
    private static final Pattern OPENING_STYLE_TAG = Pattern.compile("<style(\\s.*)?>", Pattern.CASE_INSENSITIVE);
    private static final Pattern CLOSING_STYLE_TAG = Pattern.compile("</style>", Pattern.CASE_INSENSITIVE);

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
        if (result.isEmpty()) {
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
    public void setEscaper(@Nonnull UnaryOperator<String> escaper) {
        this.escaper = escaper;
    }

    /**
     * Returns the currently active escaper.
     *
     * @return the currently active escaper
     */
    public UnaryOperator<String> getEscaper() {
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
     * @return the given string where XML control characters were replaced using {@link StringCleanup#escapeXml(String)}
     */
    public static String escapeXML(String input) {
        return Strings.cleanup(input, StringCleanup::escapeXml);
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
            return blockTrim(buffer.toString());
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        } finally {
            this.buffer = backupBuffer;
        }
    }

    /**
     * Performs a block-wise trim.
     * <p>
     * As tags might be indented, we have to determine the number of blanks in the first line (which marks the
     * initial offset). We then try to remove this amount of whitespaces from any line within the block. Finally,
     * we trim the resulting string to also get rid of leading and trailing newlines.
     * <p>
     * This will turn:
     * <pre>
     * {@code
     * &lt;x:foo&gt;
     *    This is a
     *       &lt;div&gt;sneaky&lt;/div&gt;
     *    test.
     * &lt;/x:foo&gt;
     * }
     * </pre>
     * <p>
     * into:
     *
     * <pre>
     * {@code
     * This is a
     *    &lt;div&gt;sneaky&lt;/div&gt;
     * test.
     * }
     * </pre>
     *
     * @param block the block to trim
     * @return the trimmed string
     */
    private String blockTrim(String block) {
        if (Strings.isEmpty(block) || !Character.isWhitespace(block.charAt(0))) {
            return block;
        }

        int numberOfInitialBlanks = countInitialBlanks(block);
        if (numberOfInitialBlanks == 0) {
            return block;
        } else {
            return Arrays.stream(block.split("\n"))
                         .map(line -> cutWhitespaces(line, numberOfInitialBlanks))
                         .collect(Collectors.joining("\n"))
                         .trim();
        }
    }

    private String cutWhitespaces(String line, int numWhitespaces) {
        int index = 0;
        while (numWhitespaces > 0 && index < line.length() && line.charAt(index) == ' ') {
            index++;
            numWhitespaces--;
        }

        return line.substring(index);
    }

    private int countInitialBlanks(String block) {
        int offset = 0;
        int blanks = 0;
        while (offset < block.length() && block.charAt(offset) == '\n') {
            offset++;
        }
        while (offset < block.length() && block.charAt(offset) == ' ') {
            offset++;
            blanks++;
        }

        return blanks;
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
    public DebugLevel getDebugLevel() {
        return this.debugLevel;
    }

    /**
     * Sets the debug level for rendered contents.
     *
     * @param debugLevel {@link DebugLevel} to set
     */
    public void setDebugLevel(@Nonnull DebugLevel debugLevel) {
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

    /**
     * Checks if the given guard is defined or not.
     *
     * @param guard the guard String to check
     * @return true if the guard is defined, false otherwise
     */
    public boolean hasGuard(String guard) {
        return guards.contains(guard);
    }

    /**
     * Adds the given guard to the list of defined guards.
     * @param guard the guard String to add
     */
    public void addGuard(String guard) {
        guards.add(guard);
    }
}
