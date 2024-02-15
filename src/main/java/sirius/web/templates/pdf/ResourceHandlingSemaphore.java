/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import java.util.concurrent.Semaphore;

/**
 * This class is used to limit the number of concurrent resource handling operations.
 * <p>
 * This is used to avoid overloading the system with too many concurrent resource handling operations.
 */
public class ResourceHandlingSemaphore {

    /**
     * We use only the half of the available processors to avoid overloading the system.
     */
    private static final int HALF_THE_AVAILABLE_PROCESSORS = Runtime.getRuntime().availableProcessors() / 2;
    private static Semaphore semaphore;

    private ResourceHandlingSemaphore() {
        // Prevent instantiation
    }

    /**
     * Returns the semaphore instance.
     *
     * @return the semaphore instance
     */
    public static Semaphore get() {
        if (semaphore == null) {
            semaphore = new Semaphore(HALF_THE_AVAILABLE_PROCESSORS);
        }
        return semaphore;
    }
}
