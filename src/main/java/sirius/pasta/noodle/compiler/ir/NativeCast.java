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

import java.lang.reflect.Type;

/**
 * Performs a native (normal) cast to the desired type.
 * <p>
 * The main purpose is of course to specify the type for the compiler, however this will also be checked
 * at runtime and a <tt>ClassCastException</tt> is thrown if the cast doesn't work out.
 */
public class NativeCast extends Node {

    private Node inner;
    private final Class<?> type;

    /**
     * Creates a new cast.
     *
     * @param position the position in the source code
     * @param inner    the expression to cast
     * @param type     the type to cast to
     */
    public NativeCast(Position position, Node inner, Class<?> type) {
        super(position);
        this.inner = inner;
        this.type = type;
    }

    @Override
    public Type getGenericType() {
        return type;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        this.inner = inner.reduce(compilationContext);

        if (type.isAssignableFrom(inner.getType())) {
            compilationContext.warning(position, "Ignoring unnecessary cast from %s to %s", inner.getType(), type);
            return inner;
        }

        return super.reduce(compilationContext);
    }

    @Override
    public void emit(Assembler assembler) {
        inner.emit(assembler);
        assembler.emitPushConstant(type, position);
        assembler.emitByteCode(OpCode.OP_CAST, 0, position);
    }

    @Override
    public String toString() {
        return "NativeCast: " + inner + " as " + type;
    }
}
