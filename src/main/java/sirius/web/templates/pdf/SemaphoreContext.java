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
 * Used to limit the number of concurrent resource handling operations per belonging
 * {@link sirius.kernel.async.CallContext}.
 * <p>
 * The use case is that we want to control the number of spawned threads to avoid overloading the
 * system with too many concurrent operations.
 */
public class SemaphoreContext implements SubContext {

    /**
     * Used as the number of initial permits to avoid overloading the system with too many concurrent as each permit
     * should be used to handle a single operation.
     */
    private static final int HALF_THE_AVAILABLE_PROCESSORS =
            Math.max(Runtime.getRuntime().availableProcessors() / 2, 1);

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
