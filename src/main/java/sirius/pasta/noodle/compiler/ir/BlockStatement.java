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

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Represents a block of statements.
 */
public class BlockStatement extends Statement {

    private Type returnType;
    private List<Node> statements = new ArrayList<>();

    /**
     * Creates a new block.
     *
     * @param position the position within the source code
     */
    public BlockStatement(Position position) {
        super(position);
    }

    /**
     * Adds a new statement to the block
     *
     * @param statement          the statement to add
     * @param compilationContext the current compilation context
     */
    public void addStatement(Node statement, CompilationContext compilationContext) {
        if (returnType != null) {
            // Only report the first statement as dead code and skip all others...
            if (statements.get(statements.size() - 1) instanceof ReturnStatement) {
                compilationContext.error(position, "Dead code: Nothing after a return statement will be executed.");
            }

            return;
        }
        statements.add(statement);
        if (statement instanceof ReturnStatement) {
            returnType = statement.getGenericType();
        }
    }

    @Override
    public Type getGenericType() {
        return Objects.requireNonNullElse(returnType, void.class);
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        statements =
                statements.stream().map(statement -> statement.reduce(compilationContext)).collect(Collectors.toList());

        if (statements.size() == 1) {
            return statements.get(0);
        }

        return this;
    }

    @Override
    public void emit(Assembler assembler) {
        statements.forEach(statement -> {
            statement.emit(assembler);
            if (!(statement instanceof Statement)) {
                assembler.emitByteCode(OpCode.POP_TOP, 0, statement.position);
            }
        });
    }

    @Override
    public String toString() {
        return "BlockStatement: {\n"
               + statements.stream().map(Object::toString).collect(Collectors.joining(";\n"))
               + ";\n}\n";
    }
}
