/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.cache.ValueComputer;
import sirius.kernel.commons.Limit;
import sirius.kernel.commons.Strings;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;
import sirius.web.util.LinkBuilder;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Represents a slice of a result set which is being "paged through" and can provide filter facets.
 *
 * @param <E> the type of object in the page
 */
public class Page<E> {

    private static final int DEFAULT_PAGE_SIZE = 25;
    private static final String PARAM_START = "start";
    private static final String PARAM_QUERY = "query";
    private String query;
    private int start;
    private int total;
    private List<E> items = Collections.emptyList();
    private boolean more;
    private String duration;
    private List<Facet> facets = new ArrayList<>();
    private Supplier<List<Facet>> facetsSupplier;
    private Boolean hasFacets = null;
    private int pageSize = DEFAULT_PAGE_SIZE;

    /**
     * Specifies the query used to compute the result list.
     *
     * @param query the query string which was used to create the result set
     * @return the page itself for fluent method calls
     */
    public Page<E> withQuery(String query) {
        this.query = query;
        return this;
    }

    /**
     * Specifies the index of the first item.
     *
     * @param start the index of the first item shown on this page (relative to the overall list).
     * @return the page itself for fluent method calls
     */
    public Page<E> withStart(int start) {
        this.start = Math.max(start, 1);
        return this;
    }

    /**
     * Specifies the effective items this page contains.
     *
     * @param items the items contained in the page.
     * @return the page itself for fluent method calls
     */
    public Page<E> withItems(List<E> items) {
        this.items = new ArrayList<>(items);
        return this;
    }

    /**
     * Easy method for supplying items the page contains to avoid boiler code.
     * <p>
     * The supplier provides the {@link Limit} of the current page for easy iteration
     * over a result set.
     * <p>
     * This method should be supplied with a {@link List} which size was determined by
     * the supplied {@link Limit} as this method does also is able to determine whether
     * this page {@link Page#withHasMore(boolean) has more items to show} which are currently
     * not being displayed.
     * <p>
     * Using the supplied limit e.g. via {@link Limit#asPredicate()} for slicing the provided list results
     * in a list of size {@link Page#pageSize} + 1 for easier handling whether there are more elements to
     * determine if there is another page.
     *
     * @param itemsSupplier the supplier to supply items to the current page limited by the provided limit
     * @return the page itself for fluent method calls
     */
    public Page<E> withLimitedItemsSupplier(Function<Limit, List<E>> itemsSupplier) {
        Limit supplierLimit = new Limit(getStart() - 1, getPageSize() + 1);
        List<E> suppliedItems = itemsSupplier.apply(supplierLimit);
        if (suppliedItems.size() > supplierLimit.getMaxItems() - 1) {
            more = true;
            suppliedItems = suppliedItems.subList(0, supplierLimit.getMaxItems() - 1);
        }
        return withItems(suppliedItems);
    }

    /**
     * Specifies the duration (as string) it took to compute (query) the page items.
     *
     * @param duration the duration required to compute the page items.
     * @return the page itself for fluent method calls
     */
    public Page<E> withDuration(String duration) {
        this.duration = duration;
        return this;
    }

    /**
     * Specifies the facets available to further filter the page (or underlying data source).
     *
     * @param facets the facets available for further filtering
     * @return the page itself for fluent method calls
     */
    public Page<E> withFacets(List<Facet> facets) {
        this.facets.addAll(facets);
        this.hasFacets = null;
        return this;
    }

    /**
     * Adds a filter facet with the given name, title and translator.
     *
     * @param field      the name of the field being filtered
     * @param title      the title of the filter shown to the user
     * @param translator the trnanslater used to convert filter values to visual representations for the user
     * @return a newly created facet
     */
    public Facet addFacet(String field, String title, @Nullable ValueComputer<String, String> translator) {
        Facet facet = new Facet(title, field, null, translator);
        addFacet(facet);

        return facet;
    }

    /**
     * Adds a filter facet to this result page.
     *
     * @param facet the facet to add
     * @return the page itself for fluent method calls
     */
    public Page<E> addFacet(Facet facet) {
        if (this.facets == null) {
            this.facets = new ArrayList<>();
        }

        facets.add(facet);
        facet.parent = this;
        hasFacets = null;

        return this;
    }

    /**
     * Calculates the current page number
     *
     * @return number of the current page
     */
    public int getCurrentPageNumber() {
        return (this.start / this.pageSize) + 1;
    }

    /**
     * Binds the page to the request.
     * <p>
     * This will read <tt>start</tt>, <tt>query</tt> and all facet values from the given request.
     *
     * @param ctx the request to parse
     * @return the page itself for fluent method calls
     */
    public Page<E> bindToRequest(WebContext ctx) {
        if (ctx.get(PARAM_START).isFilled()) {
            withStart(ctx.get(PARAM_START).asInt(1));
        }
        withQuery(ctx.get(PARAM_QUERY).asString());
        for (Facet f : getFacets()) {
            f.withValue(ctx.get(f.getName()).asString());
        }

        return this;
    }

    /**
     * Specifies the supplier used to compute the facets available to further filter the page (or underlying data
     * source).
     *
     * @param facetsSupplier the facets supplier computing the facets available for further filtering
     * @return the page itself for fluent method calls
     */
    public Page<E> withFacetsSupplier(Supplier<List<Facet>> facetsSupplier) {
        this.facetsSupplier = facetsSupplier;
        return this;
    }

    /**
     * Specifies the flag which indicates if "more" items are available.
     *
     * @param more indicates if more items are available or not.
     * @return the page itself for fluent method calls
     */
    public Page<E> withHasMore(boolean more) {
        this.more = more;
        return this;
    }

    /**
     * Specifies the number of items shown per page.
     *
     * @param pageSize the number of items shown per page
     * @return the page itself for fluent method calls
     */
    public Page<E> withPageSize(int pageSize) {
        this.pageSize = pageSize;
        return this;
    }

    /**
     * Specifies the number of total items.
     *
     * @param total the number of items in the data source (applying the current filters)
     * @return the page itself for fluent method calls
     */
    public Page<E> withTotalItems(int total) {
        this.total = total;
        return this;
    }

    /**
     * Returns the query string which was used to create the result set.
     *
     * @return the query string which was used to create the result set
     */
    public String getQuery() {
        return query == null ? "" : query;
    }

    /**
     * Returns the absolute index (1 based) of the first item on this page.
     *
     * @return the one based index of the first item on this page
     */
    public int getStart() {
        return start;
    }

    /**
     * Returns the absolute index (1 based) of the last item on this page.
     *
     * @return the one based index of the last item on this page
     */
    public int getEnd() {
        return start + items.size() - 1;
    }

    /**
     * Returns the items on this page.
     *
     * @return the list of items on this page
     */
    public List<E> getItems() {
        return Collections.unmodifiableList(items);
    }

    /**
     * Returns the index of the previous page.
     *
     * @return the index of the first item of the previous page
     */
    public int getPreviousStart() {
        return Math.max(1, start - pageSize);
    }

    /**
     * Returns the index of the next page.
     *
     * @return the index of the first item of the next page
     */
    public int getNextStart() {
        return start + pageSize;
    }

    /**
     * Determines if there is a previous page.
     *
     * @return <tt>true</tt> if there is a page before this one
     */
    public boolean hasLess() {
        return start > 1;
    }

    /**
     * Determines if there is a next page.
     *
     * @return <tt>true</tt> if there are more items in the underlying result
     */
    public boolean hasMore() {
        return more;
    }

    /**
     * Returns the total number of items (if known).
     *
     * @return the total number of items
     */
    public int getTotal() {
        return total;
    }

    /**
     * Returns a string representation naming the first and last index contained on this page.
     *
     * @return a string naming the first and last index of this page
     */
    public String getRange() {
        if (getItems().isEmpty()) {
            return NLS.get("Page.noResults");
        }
        return start + " - " + getEnd();
    }

    /**
     * Returns the search duration.
     *
     * @return a string representation of the search duration used to create the underlying result set
     */
    public String getDuration() {
        return duration;
    }

    protected LinkBuilder addFacetsAndQuery(String baseUrl, String fieldToIgnore) {
        LinkBuilder linkBuilder = new LinkBuilder(baseUrl);

        linkBuilder.appendIfFilled(PARAM_QUERY, query);

        for (Facet f : getFacets()) {
            if (!Strings.areEqual(fieldToIgnore, f.getName())) {
                linkBuilder.appendIfFilled(f.getName(), f.getValue());
            }
        }

        return linkBuilder;
    }

    /**
     * Generates a link to the current page using the given baseUrl.
     *
     * @param baseUrl the base url to be used
     * @return the base url extended with all parameters (facets, query, start) to render the current page again
     */
    public String linkToCurrentPage(String baseUrl) {
        return linkToGivenStart(baseUrl, start);
    }

    private String linkToGivenStart(String baseUrl, int explicitStart) {
        return addFacetsAndQuery(baseUrl, null).appendIfFilled(PARAM_START, explicitStart <= 1 ? null : explicitStart)
                                               .toString();
    }

    /**
     * Generates a link to the previous page using the given baseUrl.
     *
     * @param baseUrl the base url to be used
     * @return the base url extended with all parameters (facets, query, start) to render the previous page
     */
    public String linkToPreviousPage(String baseUrl) {
        return linkToGivenStart(baseUrl, getPreviousStart());
    }

    /**
     * Generates a link to the next page using the given baseUrl.
     *
     * @param baseUrl the base url to be used
     * @return the base url extended with all parameters (facets, query, start) to render the next page
     */
    public String linkToNextPage(String baseUrl) {
        return linkToGivenStart(baseUrl, getNextStart());
    }

    /**
     * Generates a link to the given baseUrl with an unspecified <tt>start</tt> parameter.
     *
     * <p>
     * The generated link will end with <tt>start=</tt> and therefore a JavaScript can add
     * the desired value at runtime to link to the appropriate page.
     *
     * @param baseUrl the base url to be used
     * @return the base url extended with all parameters (facets, query) to render a configurable page.
     */
    public String linkToPageWithConfigurableStart(String baseUrl) {
        return addFacetsAndQuery(baseUrl, null).appendRaw(PARAM_START, "").toString();
    }

    /**
     * Returns all filter facets available.
     *
     * @return all filter facets of the underlying result set
     */
    public List<Facet> getFacets() {
        if (facetsSupplier != null) {
            facets.addAll(facetsSupplier.get());
            facetsSupplier = null;
            hasFacets = null;
        }
        if (hasFacets == null) {
            hasFacets = false;
            for (Facet facet : facets) {
                facet.parent = this;
                if (facet.hasItems()) {
                    hasFacets = true;
                }
            }
        }

        return Collections.unmodifiableList(facets);
    }

    /**
     * Determines if there is at least one filter facet with filter items.
     *
     * @return <tt>true</tt> if there is at least one filter facet with items
     */
    public boolean hasFacets() {
        if (hasFacets == null) {
            getFacets();
        }

        return hasFacets;
    }

    /**
     * Returns the total page size.
     * <p>
     * This not the number of items in the page but rather the general paging size currently being used.
     *
     * @return the max number of items in a page
     * @see #DEFAULT_PAGE_SIZE
     */
    public int getPageSize() {
        return pageSize;
    }
}
