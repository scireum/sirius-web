/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

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
public class BlockwiseMacro implements Macro {

    @Override
    public Class<?> getType() {
        return List.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 2
            || !Tagliatelle.isAssignableTo(args.get(0).getType(), Iterable.class)
            || !Tagliatelle.isAssignableTo(args.get(1).getType(), Integer.class)) {
            throw new IllegalArgumentException("Expected an iterable an a number as arguments");
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        List<List<Object>> blocks = new ArrayList<>();
        Iterator<Object> iter = ((Iterable<Object>) args[0].eval(ctx)).iterator();
        int blockSize = (int) args[1].eval(ctx);
        while (iter.hasNext()) {
            List<Object> block = new ArrayList<>();
            blocks.add(block);
            int currentBlock = blockSize;
            while (iter.hasNext() && currentBlock-- > 0) {
                block.add(iter.next());
            }
        }

        return blocks;
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
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
}
