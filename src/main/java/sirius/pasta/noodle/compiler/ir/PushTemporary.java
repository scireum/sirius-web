/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.VariableScoper;

import javax.annotation.Nullable;
import java.lang.reflect.Type;

/**
 * Loads the given variable on the stack.
 */
public class PushTemporary extends Node {

    private VariableScoper.Variable variable;

    /**
     * Creates a new variable access.
     *
     * @param position the position in the source code
     * @param variable the variable to access
     */
    public PushTemporary(Position position, VariableScoper.Variable variable) {
        super(position);
        this.variable = variable;
    }

    @Nullable
    @Override
    public Type getGenericType() {
        return variable.getType();
    }

    @Override
    public void emit(Assembler assembler) {
        assembler.emitByteCode(OpCode.PUSH_VARIABLE, variable.getLocalIndex(), position);
    }

    /**
     * Returns the name of the variable being accessed.
     *
     * @return the name of the variable
     */
    public String getVariableName() {
        return variable.getName();
    }

    /**
     * The local offset of the variable being accessed.
     *
     * @return the computed index of the variable
     */
    public int getVariableIndex() {
        return variable.getLocalIndex();
    }

    @Override
    public String toString() {
        return "PushTemporary: " + variable.getName();
    }
}
