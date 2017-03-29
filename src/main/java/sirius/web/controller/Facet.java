/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import com.google.common.collect.Lists;
import sirius.kernel.cache.ValueComputer;
import sirius.kernel.commons.Strings;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

/**
 * Represents a filter facet used by search / query results.
 */
public class Facet {
    protected Page<?> parent;
    private String name;
    private String title;
    private List<String> values;
    private final ValueComputer<String, String> translator;
    private List<FacetItem> items = Lists.newArrayList();

    /**
     * Creates a new faced with the given parameters.
     *
     * @param title      the visible name of the facet
     * @param field      the internal name of the facet
     * @param value      the selected value
     * @param translator the translator which provides "official" labels for filter values.
     */
    public Facet(String title,
                 String field,
                 @Nullable String value,
                 @Nullable ValueComputer<String, String> translator) {
        this.name = field;
        this.title = title;
        this.values = new ArrayList<>();
        if (value != null) {
            this.values.add(value);
        }
        this.translator = translator;
    }

    /**
     * Returns all items collected for this facet.
     *
     * @return a list of all items of this facet
     */
    public List<FacetItem> getItems() {
        return items;
    }

    /**
     * Creates a query string which can be appended to a link in order to toggle the given item.
     *
     * @param item the item to toggle
     * @return a query string (?x=y..) used to toggle the given facet item
     */
    public String createToggleQueryString(FacetItem item) {
        return parent.createQueryString(name, item.isActive() ? "" : item.getKey(), true);
    }

    /**
     * Returns the name of this facet.
     *
     * @return the internal name of this facet
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the title of this fact.
     *
     * @return the visible title of this facet
     */
    public String getTitle() {
        return title;
    }

    /**
     * Returns the selected value.
     *
     * @return the selected filter value of this facet
     */
    public String getValue() {
        return !values.isEmpty() ? values.get(0) : null;
    }

    /**
     * Adds a facet item
     *
     * @param key   the filter value of the item
     * @param title the public visible name of the item
     * @param count the number of matched for this item
     * @return the facet itself for fluent method calls
     */
    public Facet addItem(String key, String title, long count) {
        if (Strings.isFilled(key)) {
            String effectiveTitle = translator == null ? title : translator.compute(key);
            if (effectiveTitle != null) {
                items.add(new FacetItem(key,
                                        effectiveTitle,
                                        count,
                                        values.stream().anyMatch(value -> Strings.areEqual(value, key))));
            }
        }
        return this;
    }

    /**
     * Adds the given collections of items.
     *
     * @param items the items to add
     * @return the facet itself for fluent method calls
     */
    public Facet addItems(Iterable<String> items) {
        for (String item : items) {
            addItem(item, null, -1);
        }
        return this;
    }

    /**
     * Adds the enum constants of the given enum to the facet.
     *
     * @param enumClass the enum class which constants are to be added
     * @param <E>       the type of the enum
     * @return the facet itself for fluent method calls
     */
    public <E extends Enum<E>> Facet addEnumItem(Class<E> enumClass) {
        for (E item : enumClass.getEnumConstants()) {
            addItem(item.name(), item.toString(), -1);
        }
        return this;
    }

    /**
     * Determines if this facet has at least one item
     *
     * @return <tt>true</tt> if this facet has at least one item, <tt>false</tt> otherwise
     */
    public boolean hasItems() {
        return !items.isEmpty();
    }

    /**
     * Specifies the value used for this facet.
     *
     * @param value the active filter value of this facet
     * @return the facet itself for fluent method calls
     */
    public Facet withValue(String value) {
        this.values.clear();
        this.values.add(value);

        return this;
    }

    /**
     * Specifies the values used for this facet.
     *
     * @param values the active filter values of this facet
     * @return the facet itself for fluent method calls
     */
    public Facet withValues(List<String> values) {
        this.values = values;

        return this;
    }
}
