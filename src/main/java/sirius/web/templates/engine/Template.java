/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import sirius.web.templates.engine.emitter.CompositeEmitter;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Created by aha on 10.05.17.
 */
public class Template {

    protected List<Class<?>> argumentTypes = new ArrayList<>();
    protected CompositeEmitter emitter;

    public void render(Consumer<String> output, Object... args) {
        RenderContext ctx = new RenderContext(output);
        for (Object arg : args) {
            ctx.push(arg);
        }

        render(ctx);
    }

    public void render(RenderContext ctx) {
        emitter.emit(ctx);
    }
}
