/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Loops over a given {@link Iterable} or an Array and invokes the given block for each item within.
 */
public class LoopEmitter extends Emitter {

    private Callable iterableExpression;
    private Emitter loop;
    private int loopStateIndex = -1;
    private int localIndex;

    /**
     * Creates a new emitter for the given position.
     *
     * @param startOfBlock the position where the emitter was declared.
     */
    public LoopEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    /**
     * Specifies the expression to evaluate which yields the items to iterate over.
     *
     * @param iterableExpression the expression which evaluates to the list of items to process
     */
    public void setIterableExpression(Callable iterableExpression) {
        this.iterableExpression = iterableExpression;
    }

    /**
     * Sets the body to emit once per item in the list of items to output.
     *
     * @param loop the loop body to emit per item
     */
    public void setLoop(Emitter loop) {
        this.loop = loop;
    }

    @Override
    public Emitter reduce() {
        this.loop = loop.reduce();

        return this;
    }

    /**
     * Verifies the consistency of the emitter.
     *
     * @param compilationContext the context used to report errors to
     */
    public void verify(CompilationContext compilationContext) {
        if (!Iterable.class.isAssignableFrom(iterableExpression.getType()) && !iterableExpression.getType().isArray()) {
            compilationContext.error(startOfBlock, "A for loop must have an Iterable or an Array as expression");
        }
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        if (iterableExpression == null || localIndex < 0) {
            return;
        }

        Object iterable = iterableExpression.call(context);
        if (iterable == null) {
            return;
        }

        Iterable<?> items = getItems(iterable);
        LoopState loopState = new LoopState(items);
        if (loopStateIndex > -1) {
            context.writeVariable(loopStateIndex, loopState);
        }
        for (Object obj : items) {
            loopState.nextRow();
            context.writeVariable(localIndex, obj);
            loop.emit(context);
        }
    }

    private Iterable<?> getItems(Object innerExpression) {
        if (innerExpression instanceof Iterable<?> iterable) {
            return iterable;
        }
        if (innerExpression.getClass().isArray()) {
            return () -> new ArrayIterator(innerExpression);
        }
        throw new ClassCastException(innerExpression.getClass() + " is neither an Iterable<?> nor an array type");
    }

    /**
     * Returns the expression which yields the items to iterate through.
     *
     * @return the expression to iterate through
     */
    public Callable getIterableExpression() {
        return iterableExpression;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("@for (");
        sb.append(iterableExpression);
        sb.append(") {");
        sb.append(loop);
        sb.append("}");

        return sb.toString();
    }

    /**
     * Updates the stack index of the loop state.
     * <p>
     * When inlining a template, the stack has to be transferred to the callee and therefore the
     * stack indices might change.
     *
     * @param loopStateIndex the new stack index to use
     */
    public void setLoopStateIndex(int loopStateIndex) {
        this.loopStateIndex = loopStateIndex;
    }

    /**
     * Contains the stack index which contains the loop state or -1 to indicate that the loop state is unused.
     *
     * @return the target index for the loop state
     */
    public int getLoopStateIndex() {
        return loopStateIndex;
    }

    /**
     * Contains the stack index being written to.
     *
     * @return the target index to write to
     */
    public int getLocalIndex() {
        return localIndex;
    }

    /**
     * Updates the stack index being written to.
     * <p>
     * When inlining a template, the stack has to be transferred to the callee and therefore the
     * stack indices might change.
     *
     * @param localIndex the new stack index to use
     */
    public void setLocalIndex(int localIndex) {
        this.localIndex = localIndex;
    }

    private static class ArrayIterator implements Iterator<Object> {
        private final Object array;
        private int index;

        private ArrayIterator(Object array) {
            this.array = array;
            index = 0;
        }

        @Override
        public boolean hasNext() {
            return index < Array.getLength(array);
        }

        @Override
        public Object next() {
            try {
                return Array.get(array, index++);
            } catch (ArrayIndexOutOfBoundsException exception) {
                throw new NoSuchElementException(exception);
            }
        }
    }
}
