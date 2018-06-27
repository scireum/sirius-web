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
import sirius.kernel.commons.Limit;
import sirius.kernel.commons.Monoflop;
import sirius.kernel.commons.Strings;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;

import javax.annotation.Nullable;
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
    private List<Facet> facets = Lists.newArrayList();
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
        this.items = items;
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
    public Page<E> withFactes(List<Facet> facets) {
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
            this.facets = Lists.newArrayList();
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
    public Page<E> withFactesSupplier(Supplier<List<Facet>> facetsSupplier) {
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
        return items;
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

    /**
     * Creates a query string containing all filters, the search query and a <tt>start</tt> parameter selecting the
     * previous page.
     *
     * @return a query string selecting the previous page
     */
    public String createPrevPageQueryString() {
        return createQueryString(PARAM_START, String.valueOf(getPreviousStart()), false);
    }

    /**
     * Creates a query string containing all filters, the search query and a <tt>start</tt> parameter selecting the
     * next page.
     *
     * @return a query string selecting the next page
     */
    public String createNextPageQueryString() {
        return createQueryString(PARAM_START, String.valueOf(getNextStart()), false);
    }

    /**
     * Creates a query string containing all filters and the search query and a <tt>start</tt> parameter selecting the
     * current page.
     *
     * @return a query string selecting the current page
     */
    public String createQueryString() {
        return createQueryString(null, null, false);
    }

    /**
     * Creates a query string containing all filters and the search query along with a given custom field.
     *
     * @param field      the additional field to set
     * @param value      the value of the field to set
     * @param resetStart determines if the start value should be kept (<tt>false</tt>) or reset to 1 (<tt>true</tt>)
     * @return a query string containing all filters, the query and the given parameter
     */
    public String createQueryString(String field, String value, boolean resetStart) {
        StringBuilder queryStringBuilder = new StringBuilder();
        boolean fieldFound = false;
        Monoflop ampersandPlaced = Monoflop.create();
        fieldFound |= createQueryStringForFacets(field, value, queryStringBuilder, ampersandPlaced);
        if (!resetStart) {
            fieldFound |= addStartToQueryString(field, value, queryStringBuilder, ampersandPlaced);
        }
        fieldFound |= addQueryToQueryString(field, value, queryStringBuilder, ampersandPlaced);
        if (!fieldFound && Strings.isFilled(value)) {
            queryStringBuilder.append(ampersandPlaced.firstCall() ? "" : "&");
            queryStringBuilder.append(field);
            queryStringBuilder.append("=");
            queryStringBuilder.append(Strings.urlEncode(value));
        }
        return queryStringBuilder.toString();
    }

    private boolean addQueryToQueryString(String field,
                                          String value,
                                          StringBuilder queryStringBuilder,
                                          Monoflop ampersandPlaced) {
        if (PARAM_QUERY.equals(field)) {
            queryStringBuilder.append(ampersandPlaced.firstCall() ? "" : "&");
            queryStringBuilder.append("query=");
            queryStringBuilder.append(Strings.urlEncode(value));
            return true;
        }

        if (Strings.isFilled(query)) {
            queryStringBuilder.append(ampersandPlaced.firstCall() ? "" : "&");
            queryStringBuilder.append("query=");
            queryStringBuilder.append(Strings.urlEncode(query));
        }
        return false;
    }

    private boolean addStartToQueryString(String field,
                                          String value,
                                          StringBuilder queryStringBuilder,
                                          Monoflop ampersandPlaced) {
        queryStringBuilder.append(ampersandPlaced.firstCall() ? "" : "&");
        queryStringBuilder.append("start=");
        if (PARAM_START.equals(field)) {
            queryStringBuilder.append(value);
            return true;
        } else {
            queryStringBuilder.append(start);
            return false;
        }
    }

    private boolean createQueryStringForFacets(String field,
                                               String value,
                                               StringBuilder queryStringBuilder,
                                               Monoflop ampersandPlaced) {
        boolean fieldFound = false;
        for (Facet f : getFacets()) {
            if (Strings.areEqual(field, f.getName())) {
                fieldFound = true;
                if (Strings.isFilled(value)) {
                    queryStringBuilder.append(ampersandPlaced.firstCall() ? "" : "&");
                    queryStringBuilder.append(field);
                    queryStringBuilder.append("=");
                    queryStringBuilder.append(Strings.urlEncode(value));
                }
            } else if (Strings.isFilled(f.getValue())) {
                queryStringBuilder.append(ampersandPlaced.firstCall() ? "" : "&");
                queryStringBuilder.append(f.getName());
                queryStringBuilder.append("=");
                queryStringBuilder.append(Strings.urlEncode(f.getValue()));
            }
        }
        return fieldFound;
    }

    /**
     * Creates an incomplete query string to be completed by appending the start index.
     * The Query String will contain all filters and the search query except 'start', for which the value must be
     * appended.
     * <p>
     * ex.:
     * <pre>
     * var startIndex = "33";
     * $('a.config-start').attr("href", "@prefix/@baseURL?@page.createQueryStringForConfigurableStart()" + startIndex);
     * </pre>
     *
     * @return a query string missing the start value
     */
    public String createQueryStringForConfigurableStart() {
        String result = createQueryString(null, null, true);
        if (Strings.isFilled(result)) {
            return result + "&start=";
        } else {
            return "start=";
        }
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

        return facets;
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
