/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.jobs;

import com.google.common.collect.Lists;
import sirius.kernel.async.Barrier;
import sirius.kernel.async.TaskContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.web.security.UserContext;

import java.time.Instant;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * Created by aha on 30.10.15.
 */
class ManagedTaskExecution implements Runnable, ManagedTaskContext, ManagedTask {

    protected final String taskId;
    protected final String userId;
    protected final String userName;
    protected final ManagedTaskSetup setup;
    protected volatile boolean canceled;
    protected volatile boolean erroneous;
    protected State state = State.SCHEDULED;
    protected final List<TaskLogEntry> logs = Lists.newArrayList();
    protected Instant scheduled;
    protected Instant started;
    protected Instant terminated;
    protected Barrier barrier = new Barrier();

    protected ManagedTaskExecution(ManagedTaskSetup setup) {
        this.setup = setup;
        this.taskId = UUID.randomUUID().toString();
        this.userId = UserContext.getCurrentUser().getUserId();
        this.userName = UserContext.getCurrentUser().getUserName();
    }

    @Override
    public void run() {
        state = State.RUNNING;
        TaskContext.get().setAdapter(this);
        if (!canceled) {
            try {
                setup.task.accept(this);
            } catch (Throwable e) {
                handle(e);
            }
            waitForForkedTasks();
        }
    }

    private void waitForForkedTasks() {
        while(!canceled) {
            if (barrier.await(5, TimeUnit.SECONDS)) {
                return;
            }
        }
    }

    @Override
    public void log(String message) {
        synchronized (logs) {
            logs.add(new TaskLogEntry(message, TaskLogEntry.LogType.ERROR));
            while (logs.size() > 512) {
                logs.remove(0);
            }
        }
    }

    @Override
    public void trace(String message) {

    }

    @Override
    public void setState(String message) {

    }

    @Override
    public void markErroneous() {
        erroneous = true;
    }

    @Override
    public void cancel() {
        if (!canceled) {
            log("Execution has been canceled by " + UserContext.getCurrentUser().getUserName());
        }
        canceled = true;
    }

    @Override
    public boolean isActive() {
        return !canceled;
    }

    @Override
    public String getId() {
        return taskId;
    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public State getState() {
        return state == State.RUNNING && erroneous ? State.WARNINGS : state;
    }

    @Override
    public List<TaskLogEntry> getLastLogs() {
        synchronized (logs) {
            return Lists.newArrayList(logs);
        }
    }

    public void handle(Throwable throwable) {
        log(Exceptions.handle(Jobs.LOG, throwable).getMessage());
    }

    public String getTaskId() {
        return taskId;
    }

    public String getUserId() {
        return userId;
    }

    public String getUsername() {
        return userName;
    }

    public Instant getScheduled() {
        return scheduled;
    }

    public Instant getStarted() {
        return started;
    }

    public Instant getTerminated() {
        return terminated;
    }

    @Part
    private static Tasks tasks;

    @Override
    public void fork(String category, Runnable runnable) {
        barrier.add(tasks.executor(category).fork(runnable));
    }
}
