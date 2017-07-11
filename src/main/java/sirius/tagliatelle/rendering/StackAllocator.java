/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.rendering;

import sirius.kernel.commons.Strings;

/**
 * Provides a memory management facility which reuses an internal array to provide stack sections for {@link
 * LocalRenderContext render contexts}.
 */
class StackAllocator {

    private Object[] stack;
    private int freeIndex = 0;

    /**
     * Represents an empty view which is used for contexts without local variables.
     */
    public static final View EMPTY_VIEW = new View(null, 0, 0);

    /**
     * Represents a view or slice of the internal array
     */
    static class View {

        protected StackAllocator stack;
        protected int basePointer;
        protected int size;

        protected View(StackAllocator stack, int basePointer, int size) {
            this.stack = stack;
            this.basePointer = basePointer;
            this.size = size;
        }

        /**
         * Reads the local location.
         *
         * @param index the local index to access
         * @return the value stored at the local location
         */
        public Object readLocal(int index) {
            return stack.stack[basePointer + index];
        }

        /**
         * Writes the local location.
         *
         * @param index the local index to access
         * @param value the value to store at the local location
         */
        public void writeLocal(int index, Object value) {
            stack.stack[basePointer + index] = value;
        }

        @Override
        public String toString() {
            if (stack == null) {
                return "EMPTY STACK";
            }

            return "BP: " + basePointer + ", Size: " + size + " of: " + stack;
        }
    }

    /**
     * Allocates the given number of locals on the internal array.
     * <p>
     * If the array is too small, it is resized appropriately.
     *
     * @param size the number of variables to manage
     * @return a view of the internal array which provides the given number of local variable locations
     */
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

    /**
     * Releases the given view.
     *
     * @param view the view which is expected to be the last allocated one
     */
    public void free(View view) {
        if (view.stack == null) {
            return;
        }

        if (!this.equals(view.stack) || view.basePointer + view.size != freeIndex) {
            throw new IllegalArgumentException(Strings.apply(
                    "Memory corruption! FreeIndex: %s, BasePointer: %s, Size: %s",
                    freeIndex,
                    view.basePointer,
                    view.size));
        }

        freeIndex -= view.size;
    }

    @Override
    public String toString() {
        return "Stack: Size: " + stack.length + ", Free Index: " + freeIndex;
    }
}
