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
 * Provides an execution context for managed tasks.
 * <p>
 * Managed tasks are started via {@link ManagedTasks#createManagedTaskSetup(String)}. The subsequently started task is
 * given an instance of <tt>ManagedTaskContext</tt> to interact with the execution system.
 *
 * @see ManagedTask
 */
public interface ManagedTaskContext extends TaskContextAdapter {

    /**
     * Logs the given message unless the method is called to frequently.
     * <p>
     * This method has an internal rate limit and can therefore be used by loops etc. to report the progress
     * every now and then.
     * <p>
     * A caller can rely on the rate limit and therefore can invoke this method as often as desired. Howerver
     * one must not rely on any message to be shown.
     *
     * @param message the message to add to the logs.
     */
    void logLimited(Object message);

    /**
     * Adds a warning to the task log.
     *
     * @param message the message to log
     */
    void warn(Object message);

    /**
     * Adds an error to the task log.
     *
     * @param message the message to log
     */
    void error(Object message);

    /**
     * Blocks the current task for the given number of milliseconds.
     *
     * @param millis the number of milliseconds to block
     */
    void sleepMillis(long millis);

    /**
     * Forks a sub task in the given executor (category). Note that the managed task will run as long as any forked
     * task
     * runs.
     *
     * @param category the category / executor to run the subtask in. Note that a deadlock can be created if the sub
     *                 task is run in the same execute as the managed task as the managed task will wait until all sub
     *                 tasks are finished!
     * @param runnable the runnable to execute
     */
    void fork(String category, Runnable runnable);

    /**
     * Blocks the current thread until all forked tasks have completed.
     */
    void waitForForkedTasks();
}
