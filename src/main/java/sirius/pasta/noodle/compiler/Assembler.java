/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.pasta.noodle.InterpreterCall;
import sirius.pasta.noodle.Invocation;
import sirius.pasta.noodle.OpCode;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Provides a helper class to create bytecodes.
 * <p>
 * This mainly simplifies generating appropriate jump instructions by supporting labels.
 */
public class Assembler {

    /**
     * Represents the maximum internal index or offset which can be encoded in an bytecode.
     * <p>
     * A bytecode is a 32 bit integer, where the 16 "MSBs" are used to encode the {@link OpCode} and
     * the lower 16 bits are used to store the index, we cannot store more than 2^16.
     */
    private static final int MAX_INDEX = (1 << 16) - 1;

    /**
     * Represents a label which can be used to generate a jump instruction to jump to the label.
     */
    public class Label {
        protected int id;
        protected int targetIP = -1;
        protected List<Integer> waitingIPs = new ArrayList<>();

        /**
         * Assigns a position to this label.
         * <p>
         * If a forward jump is created, first the label is created and a jump instruction is issued (which
         * contains a placeholder offset). Once the label is <b>defined</b>, all jump instruction will be
         * revisited and the actual offset is computed.
         */
        public void define() {
            this.targetIP = bytecode.size();
            for (Integer ip : waitingIPs) {
                bytecode.set(ip, bytecode.get(ip) | Math.abs(targetIP - ip - 1));
            }
        }

        protected void awaitDefinition(int jmpIP) {
            if (targetIP >= 0) {
                bytecode.set(jmpIP, bytecode.get(jmpIP) | Math.abs(targetIP - jmpIP - 1));
            } else {
                waitingIPs.add(jmpIP);
            }
        }
    }

    private final List<Integer> bytecode = new ArrayList<>();
    private final List<Object> constants = new ArrayList<>();
    private final List<Position> ipToPositionTable = new ArrayList<>();

    /**
     * Emits the given op code with the given index as bytecode.
     *
     * @param code     the op code to emit
     * @param index    the index to attach
     * @param position the position within the source code for which this op code was created
     */
    public void emitByteCode(OpCode code, int index, Position position) {
        if (index >= MAX_INDEX) {
            throw new IllegalArgumentException(Strings.apply(
                    "Cannot emit opcode %s for index %s as this would overflow! (Position in file: %s",
                    code,
                    index,
                    position.getLine() + ":" + position.getPos()));
        }

        bytecode.add(code.ordinal() << 16 | index);
        ipToPositionTable.add(position);
    }

    /**
     * Emits either a {@link OpCode#PUSH_CONST} or {@link OpCode#PUSH_BUILT_IN} for the given constant.
     * <p>
     * Also, if the constant isn't built-in, the value is added to the constant pool of the generated script.
     *
     * @param constant the constant to push
     * @param position the position in the souce code for which this push is generated
     */
    public void emitPushConstant(Object constant, Position position) {
        int sharedIndex = Invocation.SHARED_CONSTANT_POOL.getIndex(constant);
        if (sharedIndex >= 0) {
            emitByteCode(OpCode.PUSH_BUILT_IN, sharedIndex, position);
            return;
        }

        for (int i = 0; i < constants.size(); i++) {
            if (Objects.equals(constants.get(i), constant)) {
                emitByteCode(OpCode.PUSH_CONST, i, position);
                return;
            }
        }

        constants.add(constant);
        emitByteCode(OpCode.PUSH_CONST, constants.size() - 1, position);
    }

    /**
     * Emits a numeric coerce instruction if required.
     *
     * @param position  the position in the source code
     * @param type      the type of the value on the stack
     * @param otherType the target type expected by the subsequent bytecodes
     */
    public void coerce(Position position, Class<?> type, Class<?> otherType) {
        if (Long.class.equals(CompilationContext.autoboxClass(type)) && CompilationContext.isAssignableTo(Double.class,
                                                                                                          CompilationContext
                                                                                                                  .autoboxClass(
                                                                                                                          otherType))) {
            emitByteCode(OpCode.OP_COERCE_LONG_TO_DOUBLE, 0, position);
        } else if (Integer.class.equals(CompilationContext.autoboxClass(type))) {
            if (CompilationContext.isAssignableTo(Double.class, CompilationContext.autoboxClass(otherType))) {
                emitByteCode(OpCode.OP_COERCE_INT_TO_DOUBLE, 0, position);
            } else if (CompilationContext.isAssignableTo(Long.class, CompilationContext.autoboxClass(otherType))) {
                emitByteCode(OpCode.OP_COERCE_INT_TO_LONG, 0, position);
            }
        }
    }

    /**
     * Creates a new label to be used for jump instructions.
     * <p>
     * See {@link Label#define()} on how to assign a location to a label.
     *
     * @return the newly created label
     */
    public Label createLabel() {
        return new Label();
    }

    /**
     * Emits a jump instruction to the given label.
     * <p>
     * The actual offset is either derived from the label or updated later once it is defined.
     *
     * @param jmpCode     the op code to emit (either {@link OpCode#JMP}, {@link OpCode#JMP_BACK}
     *                    or {@link OpCode#JMP_FALSE}
     * @param destination the destination to jump to
     * @param position    the location in the source code for which the jump is generated
     */
    public void emitJump(OpCode jmpCode, Label destination, Position position) {
        emitByteCode(jmpCode, 0, position);
        destination.awaitDefinition(bytecode.size() - 1);
    }

    protected InterpreterCall build(Class<?> type, Type genericType, SourceCodeInfo sourceLookup) {
        return new InterpreterCall(bytecode, constants, type, genericType, sourceLookup, ipToPositionTable);
    }
}
