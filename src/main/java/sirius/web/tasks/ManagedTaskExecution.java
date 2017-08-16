/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.tasks;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import sirius.kernel.async.Barrier;
import sirius.kernel.async.TaskContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Doubles;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Average;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.web.security.UserContext;
import sirius.web.security.UserInfo;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.Writer;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * Internal implementation of {@link ManagedTask} and {@link ManagedTaskContext}.
 * <p>
 * This acts as a bridge between the actual execution withing {@link Tasks}, the {@link ManagedTasks} framework and the
 * task being executed itself.
 */
class ManagedTaskExecution implements Runnable, ManagedTaskContext, ManagedTask {

    protected final String taskId;
    protected final String userId;
    protected final String userName;
    protected final String tenantId;
    protected final ManagedTaskSetup setup;
    protected volatile boolean canceled;
    protected volatile boolean erroneous;
    protected State state = State.SCHEDULED;
    protected String stateString = "";
    protected final List<TaskLogEntry> logs = Lists.newArrayList();
    protected Writer writer;
    private final Map<String, Average> timings = Maps.newConcurrentMap();
    protected Instant scheduled;
    protected Instant started;
    protected Instant terminated;
    protected Barrier barrier = null;
    protected RateLimit logLimit = RateLimit.timeInterval(5, TimeUnit.SECONDS);

    @Part
    private static Tasks tasks;

    protected ManagedTaskExecution(ManagedTaskSetup setup) {
        this.setup = setup;
        this.taskId = UUID.randomUUID().toString();
        UserInfo currentUser = UserContext.getCurrentUser();
        if (currentUser.isLoggedIn()) {
            this.userId = currentUser.getUserId();
            this.userName = currentUser.getUserName();
            this.tenantId = currentUser.getTenantId();
        } else {
            this.userId = null;
            this.userName = null;
            this.tenantId = null;
        }
    }

    @Override
    public void run() {
        setState(NLS.get("ManagedTaskExecution.started"));
        started = Instant.now();
        state = State.RUNNING;
        TaskContext.get().setAdapter(this);
        if (!canceled) {
            try {
                setup.task.accept(this);
            } catch (Exception e) {
                handle(e);
            }
            waitForForkedTasks();
        }
        setState(NLS.get("ManagedTaskExecution.completed"));
    }

    @Override
    public void waitForForkedTasks() {
        if (barrier == null) {
            return;
        }
        log(TaskLogEntry.LogType.NORMAL, NLS.get("ManagedTaskExecution.waitingForForkedTasks"));
        setState(NLS.get("ManagedTaskExecution.waitingForForkedTasks"));
        while (!canceled) {
            if (barrier.await(5, TimeUnit.SECONDS)) {
                return;
            }
        }
    }

    @Override
    public boolean isErroneous() {
        return erroneous;
    }

    @Override
    public void log(String message) {
        log(TaskLogEntry.LogType.NORMAL, message);
    }

    @Override
    public void setLogWriter(@Nullable Writer writer) {
        synchronized (logs) {
            this.writer = writer;
        }
    }

    @Override
    public void logLimited(Object message) {
        if (logLimit.check()) {
            log(TaskLogEntry.LogType.NORMAL, message);
        }
    }

    private void log(TaskLogEntry.LogType type, Object message) {
        synchronized (logs) {
            TaskLogEntry taskLogEntry = new TaskLogEntry(NLS.toUserString(message), type);
            if (writer != null) {
                try {
                    writer.append(taskLogEntry.toString());
                    writer.append("\n");
                } catch (IOException e) {
                    Exceptions.ignore(e);
                }
            }
            logs.add(taskLogEntry);
            while (logs.size() > 512) {
                logs.remove(0);
            }
        }
    }

    @Override
    public void warn(Object message) {
        log(TaskLogEntry.LogType.WARN, message);
    }

    @Override
    public void error(Object message) {
        log(TaskLogEntry.LogType.ERROR, message);
        markErroneous();
    }

    @Override
    public void trace(String message) {
        log(TaskLogEntry.LogType.TRACE, message);
    }

    @Override
    public void sleepMillis(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            error(Exceptions.createHandled().error(e).handle().getMessage());
        }
    }

    @Override
    public void setState(String message) {
        this.stateString = message == null ? "" : message;
    }

    @Nullable
    @Override
    public String getStateString() {
        return stateString;
    }

    @Override
    public void markErroneous() {
        erroneous = true;
    }

    @Override
    public void inc(String counter) {
        addTiming(counter, 0);
    }

    @Override
    public void addTiming(String counter, long timeMillis) {
        Average avg = timings.computeIfAbsent(counter, key -> new Average());
        avg.addValue(timeMillis);
    }

    @Override
    public List<Tuple<String, String>> getTimings() {
        List<Tuple<String, String>> result = Lists.newArrayListWithCapacity(timings.size());
        for (Map.Entry<String, Average> e : timings.entrySet()) {
            if (!Doubles.isZero(e.getValue().getAvg())) {
                result.add(Tuple.create(e.getKey(),
                                        e.getValue().getCount()
                                        + " ("
                                        + NLS.toUserString(e.getValue().getAvg())
                                        + "ms)"));
            } else {
                result.add(Tuple.create(e.getKey(), String.valueOf(e.getValue().getCount())));
            }
        }

        return result;
    }

    @Override
    public void cancel() {
        if (hasCurrentUserAccess()) {
            if (!canceled) {
                warn(NLS.fmtr("ManagedTask.canceled").set("user", UserContext.getCurrentUser().getUserName()).format());
            }
            canceled = true;
        }
    }

    @Override
    public boolean isActive() {
        return !canceled && tasks.isRunning();
    }

    @Override
    public String getId() {
        return taskId;
    }

    @Override
    public String getName() {
        return setup.name;
    }

    @Override
    public String getCategory() {
        return setup.category;
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
        log(Exceptions.handle(ManagedTasks.LOG, throwable).getMessage());
    }

    public String getTaskId() {
        return taskId;
    }

    @Override
    public String getUserId() {
        return userId;
    }

    @Override
    public String getUsername() {
        return userName;
    }

    @Override
    public String getTenantId() {
        return tenantId;
    }

    @Override
    public Instant getScheduled() {
        return scheduled;
    }

    @Override
    public Instant getStarted() {
        return started;
    }

    @Override
    public Instant getTerminated() {
        return terminated;
    }

    @Override
    public void fork(String category, Runnable runnable) {
        if (barrier == null) {
            barrier = new Barrier();
        }
        barrier.add(tasks.executor(category).fork(runnable));
    }

    protected boolean hasCurrentUserAccess() {
        UserInfo user = UserContext.getCurrentUser();
        return user.hasPermission(ManagedTasks.PERMISSION_ALL_TASKS) || Strings.areEqual(user.getUserId(), userId) || (
                Strings.isFilled(user.getTenantId())
                && Strings.areEqual(user.getTenantId(), tenantId));
    }
}
