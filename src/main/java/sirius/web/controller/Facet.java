/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.cache.ValueComputer;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Represents a filter facet used by search / query results.
 */
public class Facet {
    protected Page<?> parent;
    private final String name;
    private final String title;
    protected final List<String> values = new ArrayList<>();
    private ValueComputer<String, String> translator;
    private boolean facetCollapsingEnabled = false;
    private int maxVisibleFacetItems;
    private final List<FacetItem> items = new ArrayList<>();

    @Deprecated
    private FacetRange facetRange;

    /**
     * Creates a new faced with the given parameters.
     *
     * @param title      the visible name of the facet
     * @param field      the internal name of the facet
     * @param value      the selected value
     * @param translator the translator which provides "official" labels for filter values.
     * @deprecated Use the constructor which doesn't require a value - as this is most probably supplied later anyway.
     */
    @Deprecated
    public Facet(String title,
                 String field,
                 @Nullable String value,
                 @Nullable ValueComputer<String, String> translator) {
        this.name = field;
        this.title = title;
        if (value != null) {
            this.values.add(value);
        }
        this.translator = translator;
    }

    /**
     * Creates a new facet with the given parameters.
     *
     * @param title the visible name of the facet
     * @param field the internal name of the facet
     */
    public Facet(String title, String field) {
        this.name = field;
        this.title = title;
    }

    /**
     * Specifies the value translator to use.
     *
     * @param translator the translator which computes the visible name for a given facet item key.
     * @return the facet itself for fluent method calls
     */
    public Facet withTranslator(ValueComputer<String, String> translator) {
        this.translator = translator;
        return this;
    }

    /**
     * Returns all items collected for this facet.
     *
     * @return a list of all items of this facet
     */
    @SuppressWarnings("AssignmentOrReturnOfFieldWithMutableType")
    @Explain("We want to provide mutable access here.")
    public List<FacetItem> getAllItems() {
        // Note that we intentionally return the list here as the list of items might be filtered after a
        // query or aggregation has been performed.
        return items;
    }

    /**
     * Returns all visible items collected for this facet.
     *
     * @return a list of all visible items of this facet
     */
    public List<FacetItem> getItems() {
        if (!hasHiddenItems()) {
            return Collections.unmodifiableList(items);
        }

        return items.subList(0, maxVisibleFacetItems);
    }

    /**
     * Returns all hidden items collected for this facet.
     *
     * @return a list of all hidden items of this facet, or an empty list if there are none
     */
    public List<FacetItem> getHiddenItems() {
        if (!hasHiddenItems()) {
            return Collections.emptyList();
        }

        return items.subList(maxVisibleFacetItems, items.size());
    }

    /**
     * Creates a link based on the given baseUrl which toggles the given item.
     *
     * @param baseUrl the base url to use for the link
     * @param item    the item to toggle
     * @return a link which contains all parameters of the surrounding {@link Page} (other facets, query) but which toggles
     * the value for this face. Also the start will be reset, as everything else would be counter intuitive
     */
    public String linkToPageWithToggledItem(String baseUrl, FacetItem item) {
        return parent.addFacetsAndQuery(baseUrl, name)
                     .appendIfFilled(name, item.isActive() ? "" : item.getKey())
                     .toString();
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
     * @param count the number of matched for this item or <b>-1</b> if no proper count is available
     * @return the facet itself for fluent method calls
     */
    public Facet addItem(String key, String title, int count) {
        if (Strings.isFilled(key)) {
            String effectiveTitle = translator == null ? title : translator.compute(key);
            if (effectiveTitle != null) {
                items.add(new FacetItem(this, key, effectiveTitle, count));
            }
        }
        return this;
    }

    /**
     * Removes the given item from this facet.
     *
     * @param item the item to remove
     * @return the facet itself for fluent method calls.
     */
    @SuppressWarnings("squid:S2250")
    @Explain("The list of items is usually small, therefore no performance hotspot is expected.")
    public Facet removeItem(FacetItem item) {
        items.remove(item);
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
        return addEnumItem(enumClass.getEnumConstants());
    }

    /**
     * Adds the given enum constants of the given enum to the facet.
     *
     * <p>
     * In contrast to {@link #addEnumItem(Class)}, this permits to only add a list of selected enum constants.
     *
     * @param constants the enum constants to add
     * @param <E>       the type of the enum
     * @return the facet itself for fluent method calls
     */
    @SafeVarargs
    public final <E extends Enum<E>> Facet addEnumItem(E... constants) {
        Arrays.stream(constants).forEach(e -> addItem(e.name(), e.toString(), -1));
        return this;
    }

    /**
     * Enables or disables collapsing for this facet.
     *
     * @param facetCollapsingEnabled <tt>true</tt> to enable facet collapsing, <tt>false</tt> otherwise
     * @return the facet itself for fluent method calls
     */
    public Facet withFacetCollapsingEnabled(boolean facetCollapsingEnabled) {
        this.facetCollapsingEnabled = facetCollapsingEnabled;

        return this;
    }

    /**
     * Determines if collapsing is enabled for this facet.
     *
     * @return <tt>true</tt> if facet collapsing is enabled, <tt>false</tt> otherwise
     */
    public boolean isFacetCollapsingEnabled() {
        return facetCollapsingEnabled;
    }

    /**
     * Sets the number of visible facet items.
     *
     * @param maxVisibleFacetItems the number of visible facet items
     * @return the facet itself for fluent method calls
     */
    public Facet withMaxVisibleFacetItems(int maxVisibleFacetItems) {
        this.maxVisibleFacetItems = maxVisibleFacetItems;

        return this;
    }

    /**
     * Returns the maximum number of items that should be visible.
     *
     * @return the maximum number of visible facet items
     */
    public int getMaxVisibleFacetItems() {
        return maxVisibleFacetItems;
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
     * Determines if this facet has at least one hidden item
     *
     * @return <tt>true</tt> if this facet has at least one hidden item, <tt>false</tt> otherwise
     */
    public boolean hasHiddenItems() {
        if (!facetCollapsingEnabled) {
            return false;
        }

        // Don't hide anything when items are selected
        if (!values.isEmpty()) {
            return false;
        }

        // Don't hide anything when only hide one item would be hidden
        if (items.size() == maxVisibleFacetItems + 1) {
            return false;
        }

        return items.size() > maxVisibleFacetItems;
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
        this.values.clear();
        this.values.addAll(values);

        return this;
    }

    /**
     * Returns the used facet range.
     * <p>
     * Note: This will be deprecated soon.
     *
     * @return the facet range
     */
    @SuppressWarnings("deprecation")
    public FacetRange getRange() {
        return facetRange;
    }

    /**
     * Specifies the facet range to use for this facet.
     *
     * @param facetRange the facet range to use
     * @return the facet itself for fluent method calls
     * @deprecated FacetRange has been deprecated.
     */
    @Deprecated
    public Facet withRange(FacetRange facetRange) {
        this.facetRange = facetRange;

        return this;
    }

    /**
     * Determines if this facet has a range for filtering values.
     *
     * @return <tt>true</tt> if this facet has a range, <tt>false</tt> otherwise
     */
    public boolean hasRange() {
        return facetRange != null;
    }
}
