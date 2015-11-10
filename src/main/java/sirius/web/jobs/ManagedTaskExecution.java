/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.jobs;

import com.google.common.collect.Lists;
import sirius.kernel.async.TaskContext;
import sirius.kernel.health.Exceptions;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

/**
 * Created by aha on 30.10.15.
 */
class ManagedTaskExecution implements Runnable, ManagedTaskContext, ManagedTask {

    protected final String taskId;
    protected final ManagedTaskSetup setup;
    protected volatile boolean canceled;
    protected volatile boolean erroneous;
    protected List<TaskLogEntry> logs = Lists.newArrayList();
    protected Instant scheduled;
    protected Instant started;
    protected Instant terminated;

    protected ManagedTaskExecution(ManagedTaskSetup setup) {
        this.setup = setup;
        this.taskId = UUID.randomUUID().toString();
    }

    @Override
    public void run() {
        TaskContext.get().setAdapter(this);
        if (!canceled) {
            try {
                setup.task.accept(this);
            } catch (Throwable e) {
                handle(e);
            }
        }
    }

    @Override
    public void log(String message) {
        synchronized (logs) {
            logs.add(new TaskLogEntry(message, TaskLogEntry.LogType.ERROR));
            while(logs.size() > 512) {
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

    }

    @Override
    public void cancel() {

    }

    @Override
    public boolean isActive() {
        return false;
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
    public String getState() {
        return null;
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
}
