/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

/**
 * Created by aha on 16.05.17.
 */
class RenderStack {

    private Object[] stack;
    private int freeIndex = 0;

    public static final View EMPTY_VIEW = new View(null, 0, 0);

    static class View {
        protected RenderStack stack;
        protected int basePointer;
        protected int size;

        protected View(RenderStack stack, int basePointer, int size) {
            this.stack = stack;
            this.basePointer = basePointer;
            this.size = size;
        }

        public Object readLocal(int index) {
            return stack.stack[basePointer + index];
        }

        public void writeLocal(int index, Object value) {
            stack.stack[basePointer + index] = value;
        }
    }


    public View alloc(int size) {
        if (size == 0) {
            return EMPTY_VIEW;
        }
        if (stack == null) {
            stack = new Object[8];
        }
        while (stack.length < freeIndex + size) {
            Object[] newStack = new Object[stack.length + 16];
            System.arraycopy(stack, 0, newStack, 0, stack.length);
            stack = newStack;
        }

        View view = new View(this, freeIndex, size);
        freeIndex += size;

        return view;
    }

    public void free(View view) {
        if (view.stack == null) {
            return;
        }

        if (!this.equals(view.stack) || view.basePointer + view.size != freeIndex) {
            throw new IllegalArgumentException("TODO"); //TODO
        }

        freeIndex -= view.size;
    }
}
