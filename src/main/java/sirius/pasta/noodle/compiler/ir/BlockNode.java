/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Represents a block of statements.
 */
public class BlockNode extends Node {

    private List<Node> statements = new ArrayList<>();

    /**
     * Creates a new block.
     *
     * @param position the position within the source code
     */
    public BlockNode(Position position) {
        super(position);
    }

    /**
     * Adds a new statement to the block
     *
     * @param statement the statement to add
     */
    public void addStatement(Node statement) {
        statements.add(statement);
    }

    @Override
    public Class<?> getType() {
        return void.class;
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
        statements.forEach(statement -> statement.emit(assembler));
    }

    @Override
    public String toString() {
        return "BlockNode: {\n" + statements.stream().map(Object::toString).collect(Collectors.joining(";\n")) + ";\n}\n";
    }
}
