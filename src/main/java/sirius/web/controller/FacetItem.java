/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.commons.Strings;

/**
 * Represents a single item of a {@link Facet}.
 */
public class FacetItem {

    private final Facet facet;
    private final String key;
    private final String title;
    private int count;
    private boolean forceActive;

    protected FacetItem(Facet facet, String key, String title, int count) {
        this.facet = facet;
        this.key = key;
        this.title = title;
        this.count = count;
    }

    /**
     * Returns the content value represented by this item.
     *
     * @return the key or content represented by this item
     */
    public String getKey() {
        return key;
    }

    /**
     * Returns the title shown to the user when displaying this item.
     *
     * @return the user representation of this item
     */
    public String getTitle() {
        return title;
    }

    /**
     * Returns the number of matches of the represented key.
     *
     * @return the number of matches
     */
    public int getCount() {
        return count;
    }

    /**
     * Permits to update the count of this facet item.
     * <p>
     * This might be used if a facet is built first and later updated using aggregation results of a query.
     *
     * @param count the new count to apply
     * @return the item itself for fluent method calls
     */
    public FacetItem withCount(int count) {
        this.count = count;
        return this;
    }

    /**
     * Forces the item to be active, even if its value isn't selected in the underlying {@link Facet}.
     * <p>
     * This might be used if this item represents the default value which isn't <tt>null</tt> internally.
     *
     * @return the item itself for fluent method calls
     */
    public FacetItem forceActive() {
        this.forceActive = true;
        return this;
    }

    /**
     * Determines if this item is currently an active filter.
     *
     * @return <tt>true</tt> if this is currently an active filter, <tt>false</tt> otherwise
     */
    public boolean isActive() {
        return forceActive || facet.values.stream().anyMatch(value -> Strings.areEqual(value, key));
    }

    @Override
    public String toString() {
        return title + " (" + key + ")";
    }
}
