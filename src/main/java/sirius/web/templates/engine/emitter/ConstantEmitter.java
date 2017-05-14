/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.emitter;

import sirius.kernel.commons.Strings;
import sirius.web.templates.engine.RenderContext;

/**
 * Created by aha on 10.05.17.
 */
public class ConstantEmitter implements Emitter {

    public static final ConstantEmitter EMPTY = new ConstantEmitter();

    private String value = "";

    public void append(String stringToAppend) {
        value += stringToAppend;
    }

    @Override
    public void emit(RenderContext context) {
        context.output(value);
    }

    @Override
    public String toString() {
        return Strings.limit(value, 30);
    }
}
