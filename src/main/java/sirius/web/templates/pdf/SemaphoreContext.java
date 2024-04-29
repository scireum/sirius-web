/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import sirius.kernel.async.SubContext;

import java.util.concurrent.Semaphore;

/**
 * This class is used to limit the number of concurrent resource handling operations per belonging CallContext.
 * <p>
 * The usecase is that we want to control the number of spawned threads to avoid overloading the
 * system with too many concurrent operations.
 */
public class SemaphoreContext implements SubContext {

    /**
     * We use only the half of the available processors to avoid overloading the system.
     */
    private static final int HALF_THE_AVAILABLE_PROCESSORS = Runtime.getRuntime().availableProcessors() / 2;

    private final Semaphore semaphore;

    /**
     * Initializes a new semaphore context which uses a semaphore to limit the number of concurrent resource handling.
     */
    public SemaphoreContext() {
        this.semaphore = new Semaphore(HALF_THE_AVAILABLE_PROCESSORS);
    }

    public Semaphore getSemaphore() {
        return this.semaphore;
    }

    @Override
    public SubContext fork() {
        return this;
    }

    @Override
    public void detach() {
        // GC will take care of the semaphore
    }
}
