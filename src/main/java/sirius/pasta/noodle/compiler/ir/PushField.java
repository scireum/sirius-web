/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.TypeTools;
import sirius.pasta.noodle.sandbox.SandboxMode;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;

/**
 * Pushes a Java field onto the stack.
 */
public class PushField extends Node {

    private Node selfExpression;
    private final Field field;

    /**
     * Creates a new node for the given field.
     *
     * @param position the position in the source code
     * @param field    the field to push
     */
    public PushField(CompilationContext compilationContext, Position position, Field field) {
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
    }

    public Node getSelfExpression() {
        return selfExpression;
    }

    public void setSelfExpression(Node selfExpression) {
        this.selfExpression = selfExpression;
    }

    @Override
    public boolean isConstant() {
        return Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers());
    }

    @Override
    public Object getConstantValue() {
        try {
            return field.get(null);
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(Log.SYSTEM)
                            .error(e)
                            .withSystemErrorMessage("Failed to inline a constant value %s: %s (%s)", field.toString())
                            .handle();
        }
    }

    @Override
    public Type getGenericType() {
        if (selfExpression == null) {
            return field.getGenericType();
        } else {
            return new TypeTools(selfExpression.getGenericType()).simplify(field.getGenericType());
        }
    }

    @Override
    public void emit(Assembler assembler) {
        if (selfExpression != null) {
            selfExpression.emit(assembler);
        }
        field.setAccessible(true);
        assembler.emitPushConstant(field, position);
        assembler.emitByteCode(OpCode.PUSH_FIELD, 0, position);
    }

    public Field getField() {
        return field;
    }
}
