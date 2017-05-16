/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.emitter;

import sirius.web.templates.tagliatelle.LocalRenderContext;

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
    public void emit(LocalRenderContext context) {
        context.output(value);
    }

    @Override
    public String toString() {
        return value;
    }
}
