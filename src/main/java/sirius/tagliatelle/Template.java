/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Part;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.rendering.GlobalRenderContext;
import sirius.tagliatelle.rendering.LocalRenderContext;
import sirius.tagliatelle.rendering.RenderException;
import sirius.web.templates.Resource;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents a compiled template which can be rendered.
 */
public class Template {

    protected String name;
    protected Resource resource;
    protected Emitter emitter;
    protected List<TemplateArgument> arguments = new ArrayList<>();
    protected Map<String, String> pragmas;
    private long compilationTimestamp = System.currentTimeMillis();
    private int stackDepth;

    @Part
    private static Tagliatelle engine;

    /**
     * Creates a new template with the given name (full path) and resource.
     *
     * @param name     the name of the template
     * @param resource the resource which is used as input for the compiler
     */
    public Template(String name, Resource resource) {
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
     * @param arg the argument to add
     */
    public void addArgument(TemplateArgument arg) {
        arguments.add(arg);
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
     * @param args the arguments to supply
     * @return the result of the emitters contained in the template
     * @throws RenderException in case of an error when creating the output
     */
    public String renderToString(Object... args) throws RenderException {
        GlobalRenderContext ctx = engine.createRenderContext();
        applyArguments(ctx.createContext(this), args);

        render(ctx);

        return ctx.toString();
    }

    /**
     * Returns the template using the given context and arguments.
     *
     * @param ctx  the context used to render the template
     * @param args the arguments being supplied to the template
     * @throws RenderException in case of an error when creating the output
     */
    public void render(GlobalRenderContext ctx, Object... args) throws RenderException {
        if (getEffectiveFileName().endsWith(".html") || getEffectiveFileName().endsWith(".xml")) {
            ctx.setEscaper(GlobalRenderContext::escapeXML);
        }
        LocalRenderContext context = ctx.createContext(this);
        applyArguments(context, args);
        renderWithContext(context);
    }

    private void applyArguments(LocalRenderContext ctx, Object[] args) throws RenderException {
        int index = 0;
        for (TemplateArgument arg : arguments) {
            Object argumentValue;
            if (index < args.length) {
                argumentValue = args[index];
            } else {
                if (arg.getDefaultValue() != null) {
                    argumentValue = arg.getDefaultValue().eval(ctx);
                } else {
                    throw RenderException.create(ctx,
                                                 new IllegalArgumentException(Strings.apply(
                                                         "The required parameter '%s' must not be empty.",
                                                         arg.getName())));
                }
            }

            if (!Tagliatelle.isAssignable(argumentValue, arg.getType())) {
                throw RenderException.create(ctx,
                                             new IllegalArgumentException(Strings.apply(
                                                     "An invalid value was supplied for parameter '%s'. Expected was: %s, Given was: %s",
                                                     arg.getName(),
                                                     arg.getType(),
                                                     argumentValue == null ? "null" : argumentValue.getClass())));
            }

            ctx.setLocal(index, argumentValue);
            index++;
        }
    }

    /**
     * Renders the template with the given local context.
     *
     * @param ctx the local render context
     * @throws RenderException in case of an error when creating the output
     */
    public void renderWithContext(LocalRenderContext ctx) throws RenderException {
        emitter.emit(ctx);
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
        return arguments;
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
}
