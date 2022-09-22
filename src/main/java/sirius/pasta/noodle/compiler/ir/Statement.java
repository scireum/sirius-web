/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import sirius.kernel.tokenizer.Position;

import java.lang.reflect.Type;

/**
 * Marks special nodes which do not leave a value on the stack.
 */
public abstract class Statement extends Node {

    protected Statement(Position position) {
        super(position);
    }

    @Override
    public Type getGenericType() {
        return void.class;
    }

}
