/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import java.util.List;

/**
 * Determines the state of a loop emitter.
 * <p>
 * An instance of this can be obtained by providing a variable name for "state" in a {@link sirius.pasta.tagliatelle.tags.ForTag}.
 */
public class LoopState {

    /**
     * Contains the total number of items to iterate over or -1 if the size is not known in advance
     */
    private final int totalItems;

    /**
     * Contains the <b>one based</b> index of the current row.
     */
    private int currentRow;

    /**
     * Creates a new instance for the base collection.
     * <p>
     * If the given collection is a list, we can also determine if the current row is the last one.
     *
     * @param items the collection to iterate over
     */
    protected LoopState(Iterable<?> items) {
        if (items instanceof List<?>) {
            this.totalItems = ((List<?>) items).size();
        } else {
            this.totalItems = -1;
        }
    }

    /**
     * Used by the {@link LoopEmitter} to indicate that a new row is rendered.
     */
    protected void nextRow() {
        currentRow++;
    }

    /**
     * Returns the index of the current row.
     * <p>
     * Note that this is one-based and therefore counts <tt>1, 2, 3, ...</tt>
     *
     * @return the one-based index of the current row
     */
    public int getRowIndex() {
        return currentRow;
    }

    /**
     * Determines if the first row is currently being rendered.
     *
     * @return <tt>true</tt> if the first row is currently rendered, <tt>false</tt> otherwise.
     */
    public boolean isFirst() {
        return currentRow == 1;
    }

    /**
     * Determines if a successive  row is currently being rendered.
     * <p>
     * This is the inverse to {@link #isFirst()}.
     *
     * @return <tt>true</tt> if a successive row is currently rendered, <tt>false</tt> otherwise.
     */
    public boolean isSuccessive() {
        return currentRow != 1;
    }

    /**
     * Determines if the last row is currently being rendered.
     *
     * @return <tt>true</tt> if the last row is currently rendered, <tt>false</tt> otherwise.
     * @throws UnsupportedOperationException if the total number of items is not known in advance
     */
    public boolean isLast() {
        if (totalItems == -1) {
            throw new UnsupportedOperationException("isLast is only supported for List collections");
        }
        return currentRow == totalItems;
    }

    /**
     * Determines if the an even row is currently being rendered.
     *
     * @return <tt>true</tt> if an even row is currently rendered, <tt>false</tt> otherwise.
     */
    public boolean isEven() {
        return isNth(2);
    }

    /**
     * Determines if the an odd row is currently being rendered.
     *
     * @return <tt>true</tt> if an odd row is currently rendered, <tt>false</tt> otherwise.
     */
    public boolean isOdd() {
        return !isEven();
    }

    /**
     * Determines if the n-th even row is currently being rendered.
     *
     * @param n the modulo to check for
     * @return <tt>true</tt> if the current row index modulo n is zero, <tt>false</tt> otherwise.
     */
    public boolean isNth(int n) {
        return (currentRow % n) == 0;
    }
}
