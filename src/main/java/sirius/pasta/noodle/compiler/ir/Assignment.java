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
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.VariableScoper;

/**
 * Represents an assignment.
 */
public class Assignment extends Node {

    private final VariableScoper.Variable variable;
    private Node variableValue;

    /**
     * Creates a new assignment.
     *
     * @param position      the position in the source code
     * @param variable      the variable to assign to
     * @param variableValue the expression to assign to the variable
     */
    public Assignment(Position position, VariableScoper.Variable variable, Node variableValue) {
        super(position);
        this.variable = variable;
        this.variableValue = variableValue;
    }

    @Override
    public Class<?> getType() {
        return void.class;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        variableValue = variableValue.reduce(compilationContext);
        return super.reduce(compilationContext);
    }

    @Override
    public void emit(Assembler assembler) {
        variableValue.emit(assembler);
        assembler.emitByteCode(OpCode.POP_VARIABLE, variable.getLocalIndex(), position);
    }

    @Override
    public String toString() {
        return "Assignment: " + variable.getName() + " = " + variableValue;
    }
}
