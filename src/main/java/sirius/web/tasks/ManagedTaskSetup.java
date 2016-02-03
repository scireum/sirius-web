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
 * Created by aha on 30.10.15.
 */
public class ManagedTaskSetup {
    protected ManagedTasks managedTasks;
    protected String name;
    protected String category = Tasks.DEFAULT;
    protected Consumer<ManagedTaskContext> task;

    protected ManagedTaskSetup(ManagedTasks managedTasks, String name) {
        this.managedTasks = managedTasks;
        this.name = name;
    }

    public ManagedTaskSetup withCategory(String category) {
        this.category = category;
        return this;
    }

    public ManagedTask execute(Consumer<ManagedTaskContext> task) {
        this.task = task;
        ManagedTaskExecution wrapper = new ManagedTaskExecution(this);
        managedTasks.execute(wrapper);
        return wrapper;
    }
}
