/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

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
 * Created by aha on 10.05.17.
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
    private static Engine engine;

    public Template(String name, Resource resource) {
        this.name = name;
        this.resource = resource;
    }

    public void addArgument(TemplateArgument arg) {
        arguments.add(arg);
    }

    public void addPragma(String name, String value) {
        if (pragmas == null) {
            pragmas = new HashMap<>();
        }

        pragmas.put(name, value);
    }

    public Value getPragma(String name) {
        if (pragmas == null) {
            return Value.EMPTY;
        }

        return Value.of(pragmas.get(name));
    }

    public String renderToString(Object... args) throws RenderException {
        GlobalRenderContext ctx = engine.createRenderContext();
        applyArguments(ctx.createContext(this), args);

        render(ctx);

        return ctx.toString();
    }

    public void render(GlobalRenderContext ctx, Object... args) throws RenderException {
        LocalRenderContext context = ctx.createContext(this);
        applyArguments(context, args);
        renderWithContext(context);
    }

    public void applyArguments(LocalRenderContext ctx, Object[] args) {
        int index = 0;
        for (TemplateArgument arg : arguments) {
            Object argumentValue = null;
            if (index < args.length) {
                argumentValue = args[index];
            } else {
                if (arg.getDefaultValue() != null) {
                    argumentValue = arg.getDefaultValue().eval(ctx);
                } else {
                    //TODO warn / fail!

                }
            }

            if (!arg.getType().isAssignableFrom(argumentValue.getClass())) {
                //TODO warn / fail
            }

            ctx.setLocal(index, argumentValue);
            index++;
        }
    }

    public void renderWithContext(LocalRenderContext ctx) throws RenderException {
        emitter.emit(ctx);
    }

    public long getCompilationTimestamp() {
        return compilationTimestamp;
    }

    public List<TemplateArgument> getArguments() {
        return arguments;
    }

    public String getName() {
        return name;
    }

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

    public String getFilename() {
        return getName().substring(0, getName().length() - 6);
    }

    public Emitter getEmitter() {
        return emitter;
    }

    public void setStackDepth(int stackDepth) {
        this.stackDepth = stackDepth;
    }

    public int getStackDepth() {
        return stackDepth;
    }

    public void setEmitter(Emitter emitter) {
        this.emitter = emitter;
    }
}
