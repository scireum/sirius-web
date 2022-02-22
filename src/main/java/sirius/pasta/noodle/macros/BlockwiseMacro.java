/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Provides a macro which takes a Collection/Iterable as well as a block size and returns a list of lists each
 * containing at most the given elements.
 * <p>
 * This might be useful for layouts as it permits to output a list of rows containing a list of blocks with a
 * fixed span.
 * <p>
 * Relying on pure HTML / CSS might turn out ugly as soon as the heights of the blocks start to differ. Flexbox might
 * be a solution as well but is not fully supported on all relevant platforms yet.
 */
@Register
@PublicApi
public class BlockwiseMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return List.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 2
            || !CompilationContext.isAssignableTo(args.get(0), Iterable.class)
            || !CompilationContext.isAssignableTo(args.get(1), Integer.class)) {
            throw new IllegalArgumentException("Expected an iterable and a number as arguments");
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public Object invoke(Environment environment, Object[] args) {
        List<List<Object>> blocks = new ArrayList<>();
        Iterator<Object> iterator = ((Iterable<Object>) args[0]).iterator();
        int blockSize = (int) args[1];
        while (iterator.hasNext()) {
            List<Object> block = new ArrayList<>();
            blocks.add(block);
            int currentBlock = blockSize;
            while (iterator.hasNext() && currentBlock-- > 0) {
                block.add(iterator.next());
            }
        }

        return blocks;
    }

    @Override
    public String getDescription() {
        return "Transforms an interable into a list of lists with a given size";
    }

    @Nonnull
    @Override
    public String getName() {
        return "blockwise";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
