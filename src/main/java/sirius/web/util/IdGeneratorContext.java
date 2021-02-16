/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import sirius.kernel.async.SubContext;

import java.util.concurrent.atomic.AtomicLong;

/**
 * Provides a simple thread local id generator.
 * <p>
 * This is mainly used to generate sequences of unique IDs within Tagliatelle templates.
 */
public class IdGeneratorContext implements SubContext {

    private final AtomicLong counter = new AtomicLong();

    /**
     * Generates a new locally unique id for this thread.
     *
     * @return a counter value unique for the current thread and all subtasks forked from it.
     */
    public long generateLocalId() {
        return counter.incrementAndGet();
    }

    @Override
    public SubContext fork() {
        return this;
    }

    @Override
    public void detach() {
        // Nothing to detach as we want to keep generating unique IDs across all forked threads
    }
}
