/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.Environment;
import sirius.pasta.tagliatelle.emitter.Emitter;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import java.lang.reflect.Type;

class RenderEmitterCall implements Callable {

    private final Emitter block;

    protected RenderEmitterCall(Emitter block) {
        this.block = block;
    }

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public Type getGenericType() {
        return String.class;
    }

    @Override
    public Object call(Environment environment) {
        LocalRenderContext renderContext = (LocalRenderContext) environment;
        return renderContext.getGlobalContext().emitToString(() -> block.emit(renderContext));
    }
}
