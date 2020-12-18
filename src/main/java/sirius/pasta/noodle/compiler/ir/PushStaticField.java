/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * Pushes a static field or constant.
 */
public class PushStaticField extends Node {

    private Field field;

    /**
     * Creates a new node for the given field.
     *
     * @param position the position in the source code
     * @param field    the field to push
     */
    public PushStaticField(Position position, Field field) {
        super(position);
        this.field = field;
    }

    @Override
    public boolean isConstant() {
        return Modifier.isFinal(field.getModifiers());
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
    public Class<?> getType() {
        return field.getType();
    }

    @Override
    public void emit(Assembler assembler) {
        assembler.emitPushConstant(field, position);
        assembler.emitByteCode(OpCode.OP_PUSH_STATIC_FIELD, 0, position);
    }
}
