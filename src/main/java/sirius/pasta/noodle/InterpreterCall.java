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
     * @param sourceCodeInfo  a way to retrieve the source code in case of an error
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
    public Object call(Environment environment) throws ScriptingException {
        return new Invocation(this, environment).execute();
    }

    /**
     * Provides a human readable listing of the bytecodes.
     *
     * @return a string representation of this script
     */
    public String disassemble() {
        StringBuilder sb = new StringBuilder();
        sb.append("OpCodes\n");
        sb.append("====================\n");
        int lastLine = -1;
        int lastCol = -1;
        for (int i = 0; i < opcodes.size(); i++) {
            int instruction = opcodes.get(i);
            Position position = ipToPositionTable.get(i);
            if (position.getLine() != lastLine) {
                sb.append(Strings.apply("Line %3s: ", position.getLine()));
                sb.append(sourceCodeInfo.fetchLine(position.getLine()));
                sb.append("\n");
                lastLine = position.getLine();
                lastCol = -1;
            }

            OpCode opCode = OpCode.values()[(instruction & 0x00FF0000) >> 16];
            int index = instruction & 0x0000FFFF;
            if (lastCol != position.getPos()) {
                sb.append("          ");
                for (int p = 1; p < position.getPos(); p++) {
                    sb.append(" ");
                }
                sb.append("^\n");
                lastCol = position.getPos();
            }
            sb.append("          ");
            for (int p = 1; p < position.getPos(); p++) {
                sb.append(" ");
            }
            sb.append(i);
            sb.append(Strings.apply(": %s (%s)\n", opCode.name(), index));
        }

        if (constants != null) {
            sb.append("\n\n");
            sb.append("Constants\n");
            sb.append("====================\n");
            for (Object object : constants) {
                sb.append(object);
                sb.append("\n");
            }
        }

        return sb.toString();
    }



    @Override
    public String toString() {
        return Strings.apply("InterpreterCall: %s bytecodes returning %s", opcodes.size(), returnType.getName());
    }
}
