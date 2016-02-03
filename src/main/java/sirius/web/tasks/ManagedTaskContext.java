/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.tasks;

import sirius.kernel.async.TaskContextAdapter;

/**
 * Created by aha on 30.10.15.
 */
public interface ManagedTaskContext extends TaskContextAdapter {

    void warn(Object message);

    void error(Object message);

    void sleepMillis(long millis);

    void fork(String category, Runnable runnable);

}
