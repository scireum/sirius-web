/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.commons.Amount;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

/**
 * Represents a range filter for a {@link Facet} used for filtering on lower and/or upper bounds.
 * @deprecated This makes the API overly complex and the only use-case is about to vanish
 */
@Deprecated(since = "2021/07/01")
public class FacetRange {
    private final Amount min;
    private final Amount max;
    private Amount from = Amount.NOTHING;
    private Amount to = Amount.NOTHING;

    /**
     * Creates a new range with the given min and max values.
     *
     * @param min smallest available value
     * @param max the largest available value
     */
    public FacetRange(Amount min, Amount max) {
        this.min = min;
        this.max = max;
    }

    /**
     * Returns the smallest available value.
     *
     * @return the smallest value
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public Amount getMin() {
        return min;
    }

    /**
     * Returns the largest available value.
     *
     * @return the largest value
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public Amount getMax() {
        return max;
    }

    /**
     * Returns the lower bound for filtering.
     *
     * @return the lower bound
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public Amount getFrom() {
        return from;
    }

    /**
     * Returns the upper bound for filtering.
     *
     * @return the upper bound
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public Amount getTo() {
        return to;
    }

    /**
     * Specifies the lower bound for filtering.
     *
     * @param from the lower bound
     * @return the facet itself for fluent method calls
     */
    public FacetRange setFrom(Amount from) {
        this.from = from;

        return this;
    }

    /**
     * Specifies the upper bound for filtering.
     *
     * @param to the upper bound
     * @return the facet itself for fluent method calls
     */
    public FacetRange setTo(Amount to) {
        this.to = to;

        return this;
    }
}
