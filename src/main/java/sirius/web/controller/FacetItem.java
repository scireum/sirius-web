/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

/**
 * Represents a single item of a {@link Facet}.
 */
public class FacetItem {
    private String key;
    private String title;
    private int count;
    private boolean active;

    /**
     * Creates a new FacesItem.
     *
     * @param key    the content value represented by this item
     * @param title  the visual item used to display this item to the user
     * @param count  the number of matches found
     * @param active determines if this item is an active filter setting
     */
    public FacetItem(String key, String title, int count, boolean active) {
        this.key = key;
        this.title = title;
        this.count = count;
        this.active = active;
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
     * Sets the key or content represented by this item.
     *
     * @param key the content to set
     */
    public void setKey(String key) {
        this.key = key;
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
     * Sets the title shown to the user when displaying this item.
     *
     * @param title the new title
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * Returns the number of matches of the represented key.
     *
     * @return the number of matches
     */
    public long getCount() {
        return count;
    }

    /**
     * Sets the number of matches.
     *
     * @param count the new number of matches
     */
    public void setCount(int count) {
        this.count = count;
    }

    /**
     * Determines if this item is an currently active filter.
     *
     * @return <tt>true</tt> if this is currently an active filter, <tt>false</tt> otherwise
     */
    public boolean isActive() {
        return active;
    }

    /**
     * Sets the active flag indicating if this is currently an active filter.
     *
     * @param active <tt>true</tt> to mark this as an active filter, <tt>false</tt> otherwise
     */
    public void setActive(boolean active) {
        this.active = active;
    }

    @Override
    public String toString() {
        return title + " (" + key + ")";
    }
}
