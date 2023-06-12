/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle;

import sirius.kernel.commons.Context;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.commons.Watch;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Average;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.tagliatelle.emitter.ConstantEmitter;
import sirius.pasta.tagliatelle.emitter.Emitter;
import sirius.pasta.tagliatelle.rendering.GlobalRenderContext;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;
import sirius.pasta.tagliatelle.rendering.RenderException;
import sirius.web.resources.Resource;

import javax.annotation.Nullable;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a compiled template which can be rendered.
 */
public class Template {

    protected String name;
    protected Resource resource;
    protected Emitter emitter;
    protected List<TemplateArgument> arguments = new ArrayList<>();
    protected Map<String, String> pragmas;
    private final long compilationTimestamp = System.currentTimeMillis();
    private int stackDepth;
    private final Average renderTime = new Average();

    private static final Pattern TAGLIB_NAME = Pattern.compile("/taglib/([^/]+)/([^/]+)\\.html\\.pasta");

    @Part
    private static Tagliatelle engine;

    /**
     * Creates a new template with the given name (full path) and resource.
     *
     * @param name     the name of the template
     * @param resource the resource which is used as input for the compiler
     */
    public Template(String name, @Nullable Resource resource) {
        this.name = name;
        this.resource = resource;
    }

    /**
     * Contains the effective file name (without the .pasta suffix).
     *
     * @return the effective file name
     */
    public String getEffectiveFileName() {
        if (name.endsWith(".pasta")) {
            return name.substring(0, name.length() - 6);
        }

        return name;
    }

    /**
     * Adds an argument to the template.
     *
     * @param argument the argument to add
     */
    public void addArgument(TemplateArgument argument) {
        arguments.add(argument);
    }

    /**
     * Adds a pragma which was detected while compiling.
     *
     * @param name  the name of the pragma
     * @param value the value of the pragma
     */
    public void addPragma(String name, String value) {
        if (pragmas == null) {
            pragmas = new HashMap<>();
        }

        pragmas.put(name, value);
    }

    /**
     * Reads the pragma with the given name.
     *
     * @param name the name of the pragma to read
     * @return the value of the given pragma wrapped as {@link Value} or an empty value if no such pragma exists
     */
    public Value getPragma(String name) {
        if (pragmas == null) {
            return Value.EMPTY;
        }

        return Value.of(pragmas.get(name));
    }

    /**
     * Invokes the template and renders it into a string using the given arguments.
     *
     * @param arguments the arguments to supply
     * @return the result of the emitters contained in the template
     * @throws RenderException in case of an error when creating the output
     */
    public String renderToString(Object... arguments) throws RenderException {
        GlobalRenderContext globalRenderContext = engine.createRenderContext();
        render(globalRenderContext, arguments);

        return globalRenderContext.toString();
    }

    /**
     * Invokes the template and renders it into a string using the given arguments.
     *
     * @param context the arguments to supply
     * @return the result of the emitters contained in the template
     * @throws RenderException in case of an error when creating the output
     */
    public String renderWithParams(Context context) throws RenderException {
        GlobalRenderContext globalRenderContext = engine.createRenderContext();
        setupEscaper(globalRenderContext);
        LocalRenderContext renderContext = globalRenderContext.createContext(this);
        transferArguments(context, renderContext);
        renderWithContext(renderContext);

        return globalRenderContext.toString();
    }

    /**
     * Reads all arguments from the given map into the given render context.
     * <p>
     * Also, all given values are verified and default values are used, where required.
     *
     * @param arguments     the arguments to use
     * @param renderContext the context to fill
     * @throws RenderException in case of invalid or missing arguments
     */
    public void transferArguments(Map<String, Object> arguments, LocalRenderContext renderContext)
            throws RenderException {
        int index = 0;
        for (TemplateArgument argument : getArguments()) {
            Object argumentValue = (arguments.containsKey(argument.getName())) ?
                                   arguments.get(argument.getName()) :
                                   getDefaultValue(renderContext, argument);
            verifyArgument(renderContext, argument, argumentValue);
            renderContext.writeVariable(index, argumentValue);

            index++;
        }
    }

    /**
     * Returns the template using the given context and arguments.
     *
     * @param globalRenderContext the context used to render the template
     * @param arguments           the arguments being supplied to the template
     * @throws RenderException in case of an error when creating the output
     */
    public void render(GlobalRenderContext globalRenderContext, Object... arguments) throws RenderException {
        setupEscaper(globalRenderContext);
        LocalRenderContext context = globalRenderContext.createContext(this);
        applyArguments(context, arguments);
        renderWithContext(context);
    }

    /**
     * Checks whether the template is supposed to have an XML-like structure, based on the extension of the
     * {@linkplain #getEffectiveFileName() effective file name}.
     * <p>
     * This is obviously the case for HTML and XML files. We also treat PDF templates as such, though, as we use
     * <i>flying saucer</i> to generate PDFs — which internally renders HTML as well.
     *
     * @return <b>true</b> if the template's content is expected to have an XML-like structure, <b>false</b> else
     */
    public boolean isXmlContentExpected() {
        String effectiveFileName = getEffectiveFileName();
        return effectiveFileName.endsWith(".html") || effectiveFileName.endsWith(".xml") || effectiveFileName.endsWith(
                ".pdf");
    }

    private void setupEscaper(GlobalRenderContext globalRenderContext) {
        if (isXmlContentExpected()) {
            globalRenderContext.setEscaper(GlobalRenderContext::escapeXML);
        }
    }

    private void applyArguments(LocalRenderContext localRenderContext, Object[] arguments) throws RenderException {
        int index = 0;
        for (TemplateArgument argument : getArguments()) {
            Object argumentValue =
                    (index < arguments.length) ? arguments[index] : getDefaultValue(localRenderContext, argument);
            verifyArgument(localRenderContext, argument, argumentValue);
            localRenderContext.writeVariable(index, argumentValue);

            index++;
        }
    }

    private Object getDefaultValue(LocalRenderContext localRenderContext, TemplateArgument argument)
            throws RenderException {
        if (argument.getDefaultValue() != null) {
            try {
                return argument.getDefaultValue().call(localRenderContext);
            } catch (ScriptingException exception) {
                throw RenderException.create(localRenderContext, exception);
            }
        } else {
            throw RenderException.create(localRenderContext,
                                         new IllegalArgumentException(Strings.apply(
                                                 "The required parameter '%s' must not be empty.",
                                                 argument.getName())));
        }
    }

    private void verifyArgument(LocalRenderContext localRenderContext, TemplateArgument argument, Object argumentValue)
            throws RenderException {
        if (!CompilationContext.isAssignable(argumentValue, argument.getType())) {
            throw RenderException.create(localRenderContext,
                                         new IllegalArgumentException(Strings.apply(
                                                 "An invalid value was supplied for parameter '%s'. "
                                                 + "Expected was: %s, Given was: %s",
                                                 argument.getName(),
                                                 argument.getType(),
                                                 argumentValue == null ? "null" : argumentValue.getClass())));
        }
    }

    /**
     * Renders the template with the given local context.
     *
     * @param localRenderContext the local render context
     * @throws RenderException in case of an error when creating the output
     */
    public void renderWithContext(LocalRenderContext localRenderContext) throws RenderException {
        Watch watch = Watch.start();
        emitter.emit(localRenderContext);
        renderTime.addValue(watch.elapsedMillis());
    }

    /**
     * Determines if the contents of this template are completely constant.
     *
     * @return <tt>true</tt> if the contents are completely constant, <tt>false</tt> otherwise
     */
    public boolean isConstant() {
        return emitter instanceof ConstantEmitter;
    }

    /**
     * Returns the timestamp when the template was last recompiled.
     *
     * @return the timestamp ({@link System#currentTimeMillis()} when the template was last compiled
     */
    public long getCompilationTimestamp() {
        return compilationTimestamp;
    }

    /**
     * Returns the list of arguments expected by the template.
     *
     * @return the list of arguments of the template
     */
    public List<TemplateArgument> getArguments() {
        return Collections.unmodifiableList(arguments);
    }

    /**
     * Returns the name of the template.
     *
     * @return the name of the template
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the resource which was used as input for the template.
     *
     * @return the underlying resource of the template
     */
    public Resource getResource() {
        return resource;
    }

    /**
     * Returns the compilation timestamp as {@link LocalDateTime}.
     *
     * @return the timestamp when the template was compiled
     */
    public LocalDateTime getCompilationTime() {
        return Instant.ofEpochMilli(compilationTimestamp).atZone(ZoneId.systemDefault()).toLocalDateTime();
    }

    /**
     * Returns the timestamp when the underlying resource was last changed as {@link LocalDateTime}.
     *
     * @return the timestamp when the underlying resource was last changed
     */
    public LocalDateTime getResourceLastChanged() {
        if (resource == null) {
            return LocalDateTime.now();
        }

        return Instant.ofEpochMilli(getResource().getLastModified()).atZone(ZoneId.systemDefault()).toLocalDateTime();
    }

    /**
     * Returns how many times the template was rendered since its compilation.
     *
     * @return the number of invocations of this template
     */
    public int getNumInvocations() {
        return (int) renderTime.getCount();
    }

    /**
     * Returns the average time it took to render the template.
     *
     * @return the average rendering time in milliseconds
     */
    public double getAverageRenderTime() {
        return renderTime.getAvg();
    }

    /**
     * Resets the internal performance counters.
     */
    public void resetAverageRenderTime() {
        renderTime.getAndClear();
    }

    @Override
    public String toString() {
        if (resource == null) {
            return name;
        }

        return name + " (" + resource.getUrl() + ")";
    }

    /**
     * Returns the emitter which represents the body of the template.
     *
     * @return the body emitter of the template
     */
    public Emitter getEmitter() {
        return emitter;
    }

    /**
     * Sets the stack depth / number of local variables.
     *
     * @param stackDepth the number of local variables / arguments required at runtime
     */
    public void setStackDepth(int stackDepth) {
        this.stackDepth = stackDepth;
    }

    /**
     * Returns the stack depth required by this template.
     *
     * @return the number of arguments / local variables of this template
     */
    public int getStackDepth() {
        return stackDepth;
    }

    /**
     * Sets the body of the template.
     *
     * @param emitter the emitter which represents the body of this template
     */
    public void setEmitter(Emitter emitter) {
        this.emitter = emitter;
    }

    /**
     * Represents a short an readable name of the template used in error messages.
     * <p>
     * This will most probably either render the name as tag if it is one or just output the filename instead of the
     * full path, which is nonambiguous in the context or an error message.
     *
     * @return a possibly shortened name of the template to be used in warnings and error messages
     */
    public String getShortName() {
        Matcher matcher = TAGLIB_NAME.matcher(name);
        if (matcher.matches()) {
            return "<" + matcher.group(1) + ":" + matcher.group(2) + ">";
        } else {
            return Files.getFilenameAndExtension(name);
        }
    }
}
