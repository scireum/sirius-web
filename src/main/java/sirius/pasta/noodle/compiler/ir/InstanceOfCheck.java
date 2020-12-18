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

import javax.annotation.Nullable;

/**
 * Represents an <tt>instanceof</tt> check.
 */
public class InstanceOfCheck extends Node {
    private Node expression;
    private final Class<?> typeToCheck;

    /**
     * Creates a new <tt>instanceof</tt> check.
     *
     * @param position    the position in the source code
     * @param expression  the expression to check
     * @param typeToCheck the type to ensure
     */
    public InstanceOfCheck(Position position, Node expression, Class<?> typeToCheck) {
        super(position);
        this.expression = expression;
        this.typeToCheck = typeToCheck;
    }

    @Nullable
    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        this.expression = expression.reduce(compilationContext);

        if (expression.isConstant()) {
            return new Constant(position, typeToCheck.isAssignableFrom(expression.getConstantValue().getClass()));
        } else {
            return super.reduce(compilationContext);
        }
    }

    @Override
    public void emit(Assembler assembler) {
        expression.emit(assembler);
        assembler.emitPushConstant(typeToCheck, position);
        assembler.emitByteCode(OpCode.OP_INSTANCE_OF, 0, position);
    }

    @Override
    public String toString() {
        return "InstanceCheck: " + expression + " is " + typeToCheck;
    }
}
