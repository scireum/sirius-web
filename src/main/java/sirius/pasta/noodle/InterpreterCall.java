/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.pasta.noodle.compiler.SourceCodeInfo;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.List;

/**
 * Represents a compiled <tt>Noodle</tt> script executed by the interpreter.
 */
public class InterpreterCall implements Callable {

    private static final String SEPARATOR = "====================\n";

    protected final List<Position> ipToPositionTable;
    protected final Class<?> returnType;
    protected final List<Integer> opcodes;
    protected final List<Object> constants;
    protected final Type genericReturnType;
    protected final SourceCodeInfo sourceCodeInfo;

    /**
     * Uses the output of the assembler to create an executable interpreter call.
     *
     * @param opcodes           the opcodes to execute
     * @param constants         the list of shared constants
     * @param returnType        the return type of this call
     * @param genericReturnType the generic return type
     * @param sourceCodeInfo    a way to retrieve the source code in case of an error
     * @param ipToPositionTable a map which determines the source location for each bytecode
     */
    public InterpreterCall(List<Integer> opcodes,
                           List<Object> constants,
                           Class<?> returnType,
                           Type genericReturnType,
                           SourceCodeInfo sourceCodeInfo,
                           List<Position> ipToPositionTable) {
        this.opcodes = Collections.unmodifiableList(opcodes);
        this.constants = constants.isEmpty() ? null : Collections.unmodifiableList(constants);
        this.returnType = returnType;
        this.genericReturnType = genericReturnType;
        this.sourceCodeInfo = sourceCodeInfo;
        this.ipToPositionTable = Collections.unmodifiableList(ipToPositionTable);
    }

    @Override
    public Class<?> getType() {
        return returnType;
    }

    @Override
    public Type getGenericType() {
        return genericReturnType;
    }

    @Override
    public Object call(Environment environment) {
        return new Invocation(this, environment).execute();
    }

    /**
     * Provides a human readable listing of the bytecodes.
     *
     * @return a string representation of this script
     */
    public String disassemble() {
        StringBuilder listing = new StringBuilder();
        listing.append("OpCodes\n");
        listing.append(SEPARATOR);
        int lastLine = -1;
        int lastCol = -1;
        int opCodesForLine = 0;
        for (int i = 0; i < opcodes.size(); i++) {
            int instruction = opcodes.get(i);
            Position position = ipToPositionTable.get(i);
            if (position.getLine() != lastLine || opCodesForLine-- <= 0) {
                listing.append(Strings.apply("Line %3s: ", position.getLine()));
                listing.append(sourceCodeInfo.fetchLine(position.getLine()));
                listing.append("\n");
                lastLine = position.getLine();
                lastCol = -1;
                opCodesForLine = 2;
            }

            OpCode opCode = OpCode.values()[(instruction & 0x00FF0000) >> 16];
            int index = instruction & 0x0000FFFF;
            if (lastCol != position.getPos()) {
                listing.append("          ");
                listing.append(" ".repeat(Math.max(0, position.getPos() - 1)));
                listing.append("^\n");
                lastCol = position.getPos();
            }
            listing.append("          ");
            listing.append(" ".repeat(Math.max(0, position.getPos() - 1)));
            listing.append(i);
            listing.append(Strings.apply(": %s (%s)\n", opCode.name(), index));
        }

        if (constants != null) {
            listing.append("\n\n");
            listing.append("Constants\n");
            listing.append(SEPARATOR);
            int index = 0;
            for (Object object : constants) {
                listing.append(Strings.apply("%3s: ", index++));
                listing.append(object);
                listing.append("\n");
            }
        }

        listing.append("\n\n");
        listing.append("Shared Constants\n");
        listing.append(SEPARATOR);
        int index = 0;
        for (Object object : Invocation.SHARED_CONSTANT_POOL.getSharedConstants()) {
            listing.append(Strings.apply("%3s: ", index++));
            listing.append(object);
            listing.append("\n");
        }


        return listing.toString();
    }

    @Override
    public String toString() {
        return Strings.apply("InterpreterCall: %s bytecodes returning %s", opcodes.size(), returnType.getName());
    }
}
