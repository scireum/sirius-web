/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.tasks;

import sirius.kernel.async.Tasks;

import java.util.function.Consumer;

/**
 * A builder pattern to create a {@link ManagedTask}.
 */
@Deprecated
public class ManagedTaskSetup {
    protected ManagedTasks managedTasks;
    protected String name;
    protected String category = Tasks.DEFAULT;
    protected Consumer<ManagedTaskContext> task;

    protected ManagedTaskSetup(ManagedTasks managedTasks, String name) {
        this.managedTasks = managedTasks;
        this.name = name;
    }

    /**
     * Used to specify the executor / category of the task.
     *
     * @param category the category to execute this task in
     * @return the setup itself for fluent method calls
     * @see Tasks#executor(String)
     */
    public ManagedTaskSetup withCategory(String category) {
        this.category = category;
        return this;
    }

    /**
     * Creates the managed task for the given consumer which will be supplied with a {@link ManagedTaskContext} and
     * executed in the background.
     *
     * @param task the actual task to execute
     * @return the newly created managed task
     */
    public ManagedTask execute(Consumer<ManagedTaskContext> task) {
        this.task = task;
        ManagedTaskExecution wrapper = new ManagedTaskExecution(this);
        managedTasks.execute(wrapper);
        return wrapper;
    }
}
