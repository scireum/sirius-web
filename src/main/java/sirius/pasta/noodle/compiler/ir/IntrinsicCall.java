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

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Represensts a method call which is reflected by a single OpCode.
 */
public class IntrinsicCall extends Call {

    private final Class<?> returnType;
    private final OpCode opCode;

    /**
     * Creates a new intrinsic with the given parameters.
     *
     * @param position       the location in the source code
     * @param type           the return type of the intrinsic call
     * @param opCode         the intrinsic to invoke
     * @param parameterNodes the parameters to pass in
     */
    public IntrinsicCall(Position position, Class<?> type, OpCode opCode, Node[] parameterNodes) {
        super(position);
        this.opCode = opCode;
        this.parameterNodes = parameterNodes;
        this.returnType = type;
    }

    @Nullable
    @Override
    public Class<?> getType() {
        return returnType;
    }

    public OpCode getOpCode() {
        return opCode;
    }

    @Override
    public void emit(Assembler assembler) {
        for (int i = parameterNodes.length - 1; i >= 0; i--) {
            parameterNodes[i].emit(assembler);
        }
        assembler.emitByteCode(opCode, 0, position);
    }

    @Override
    public String toString() {
        return "IntrinsicCall: " + opCode + "(" + Arrays.stream(parameterNodes)
                                                        .map(Object::toString)
                                                        .collect(Collectors.joining(", ")) + ")";
    }
}
