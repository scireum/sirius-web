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

import java.util.Objects;

/**
 * Represents a constant expression.
 */
public class Constant extends Node {

    protected Object value;

    /**
     * Creates a new constant with the given value.
     *
     * @param position the position in the source code
     * @param value    the constant value to represent
     */
    public Constant(Position position, Object value) {
        super(position);
        this.value = value;
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
    public Class<?> getType() {
        return value == null ? void.class : value.getClass();
    }

    @Override
    public void emit(Assembler assembler) {
        assembler.emitPushConstant(value, position);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Constant that = (Constant) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return "Constant: " + value;
    }
}
