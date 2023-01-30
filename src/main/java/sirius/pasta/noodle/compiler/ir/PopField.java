/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.sandbox.SandboxMode;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * Stores a stack value in a Java field.
 */
public class PopField extends Statement {

    private Node selfExpression;
    private final Field field;
    private Node valueExpression;

    /**
     * Creates a new node for the given field.
     *
     * @param position the position in the source code
     * @param field    the field to push into
     */
    public PopField(CompilationContext compilationContext, Position position, Field field) {
        super(position);
        this.field = field;

        if (!Modifier.isPublic(field.getModifiers())) {
            if (compilationContext.getSandboxMode() == SandboxMode.ENABLED) {
                compilationContext.error(position,
                                         "The field '%s' of '%s' is not public accessible.",
                                         field.getName(),
                                         field.getDeclaringClass().getName());
            } else if (compilationContext.getSandboxMode() == SandboxMode.WARN_ONLY) {
                compilationContext.warning(position,
                                           "The field '%s' of '%s' is not public accessible.",
                                           field.getName(),
                                           field.getDeclaringClass().getName());
            }
        }

        if (Modifier.isFinal(field.getModifiers())) {
            compilationContext.error(position,
                                     "The field '%s' of '%s' is final and cannot be assigned with a value.",
                                     field.getName(),
                                     field.getDeclaringClass().getName());
        }
    }

    public Node getSelfExpression() {
        return selfExpression;
    }

    public void setSelfExpression(Node selfExpression) {
        this.selfExpression = selfExpression;
    }

    public Node getValueExpression() {
        return valueExpression;
    }

    public void setValueExpression(Node valueExpression) {
        this.valueExpression = valueExpression;
    }

    @Override
    public void emit(Assembler assembler) {
        valueExpression.emit(assembler);
        if (selfExpression != null) {
            selfExpression.emit(assembler);
        }
        field.setAccessible(true);
        assembler.emitPushConstant(field, position);
        assembler.emitByteCode(OpCode.POP_FIELD, 0, position);
    }
}
