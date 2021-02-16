/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.compiler.Assembler;

import java.lang.reflect.Type;

/**
 * Represents a raw class literal.
 * <p>
 * A raw class literal is an intermediate node. This is used if either a field is accessed like
 * {@code my.Class.FIELD} then first <tt>my.Class</tt> would be parsed as <tt>RawClassLiteral</tt>
 * and then transformed into a {@link PushField}. The same goes for a "real" class literal
 * like {@code my.Class.class} which is then simply transformed into a {@link Constant} with the
 * class as value.
 */
public class RawClassLiteral extends Node {

    private final Class<?> value;

    /**
     * Creates a new class literal.
     *
     * @param position the position in the source code
     * @param value    the class represented by this literal
     */
    public RawClassLiteral(Position position, Class<?> value) {
        super(position);
        this.value = value;
    }

    @Override
    public Type getGenericType() {
        // We return the actual class here as well to simplify the method retrieval code in MethodCall...
        return (Class<?>) getConstantValue();
    }

    @Override
    public boolean isConstant() {
        return true;
    }

    @Override
    public Object getConstantValue() {
        return value;
    }

    @Override
    public void emit(Assembler assembler) {
        // A raw class literal itself cannot be compiled into anything. If a "real" class literal is detected,
        // it would have been transformed into a Constant by now...
        throw new IllegalStateException("Cannot emit code for a raw class literal at "
                                        + position.getLine()
                                        + ":"
                                        + position.getPos());
    }

    @Override
    public String toString() {
        return "RawClassLiteral: " + value;
    }
}
