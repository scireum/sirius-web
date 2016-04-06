/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.tasks;

import sirius.kernel.nls.NLS;

import javax.annotation.Nullable;
import java.time.Instant;
import java.util.List;

/**
 * A managed task is essentially a background job or task with some metadata attached.
 * <p>
 * It can be created by {@link ManagedTasks#createManagedTaskSetup(String)} and is used to report
 * potentially long running acitivities to the administrator.
 * <p>
 * A managed task is created using a {@link ManagedTaskSetup}. The acutal runnable being executed uses a
 * {@link ManagedTaskContext} to interact with the system. A {@link ManagedTask} is the supervisor interface
 * which mainly reports the state of running tasks.
 * </p>
 */
public interface ManagedTask {

    /**
     * Enuemrates possible states of a task.
     */
    enum State {
        SCHEDULED("label-info"), RUNNING("label-success"), WARNINGS("label-warn"), TERMINATED("label-default");

        State(String labelClass) {
            this.labelClass = labelClass;
        }

        private String labelClass;

        public String getLabelClass() {
            return labelClass;
        }

        public String toString() {
            return NLS.get("ManagedTask.State." + name());
        }
    }

    /**
     * Contains the unique ID of the task.
     *
     * @return the unique ID of the task
     */
    String getId();

    /**
     * Contains the unique ID of the user that started the task.
     *
     * @return the id of the user which started the task
     */
    @Nullable
    String getUserId();

    /**
     * Returns the name of the user that started the task.
     *
     * @return the name of the user that started the task
     */
    @Nullable
    String getUsername();

    /**
     * Returns the timestamp when the task was scheduled (started in the users eye).
     *
     * @return the timestamp when the task was scheduled
     */
    Instant getScheduled();

    /**
     * Returns the timestamp when the execution of the task started.
     *
     * @return the timestamp when the execution of the task started
     */
    @Nullable
    Instant getStarted();

    /**
     * Returns the timestamp when the execution of the task finished.
     *
     * @return the timestamp when the execution of the task finished
     */
    @Nullable
    Instant getTerminated();

    /**
     * Returns a descriptive name of the task.
     *
     * @return a descriptive name of the task
     */
    String getName();

    /**
     * Returns the category (executor) in which the task was started.
     *
     * @return the name of the executor which was used to schedule and execute the task
     * @see sirius.kernel.async.Tasks#executor(String)
     */
    String getCategory();

    /**
     * Returns the state of the task.
     *
     * @return the state of the task
     */
    State getState();

    /**
     * Returns a short textual description of what the task is currently doing.
     *
     * @return a shot description of the tasks current state
     */
    @Nullable
    String getStateString();

    /**
     * Returns the latest log entries.
     * <p>
     * The log is limited to a sane number of entries to pevent excessive memory usage.
     *
     * @return a list containing the lastest log entries
     */
    List<TaskLogEntry> getLastLogs();

    /**
     * Cancels the execution of the task.
     */
    void cancel();
}
