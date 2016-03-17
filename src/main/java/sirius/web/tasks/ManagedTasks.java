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
import sirius.kernel.async.CompletionHandler;
import sirius.kernel.async.Future;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Log;
import sirius.web.security.UserContext;
import sirius.web.security.UserInfo;

import javax.annotation.Nullable;
import java.time.Instant;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Executes background tasks which can be monitored via the user interface.
 *
 * @see ManagedTask
 */
@Register(classes = ManagedTasks.class)
public class ManagedTasks {

    /**
     * Contains the log being used by this framework
     */
    public static final Log LOG = Log.get("managed-tasks");

    /**
     * This permission needed to view all running tasks. A user (being logged-in) can always see his or her own tasks.
     */
    public static final String PERMISSION_ALL_TASKS = "permission-tasks";

    private Map<String, ManagedTaskExecution> activeTasks = Collections.synchronizedMap(Maps.newLinkedHashMap());
    private RateLimit taskCleanupLimit = RateLimit.timeInterval(10, TimeUnit.SECONDS);

    @Part
    private Tasks tasks;

    /**
     * Creates a new setup which is used to start a new task.
     *
     * @param name the name of the task being started.
     * @return a setup used to start a new managed task
     */
    public ManagedTaskSetup createManagedTaskSetup(String name) {
        return new ManagedTaskSetup(this, name);
    }

    protected void execute(ManagedTaskExecution exec) {
        cleanupTasks();
        exec.scheduled = Instant.now();
        exec.state = ManagedTask.State.SCHEDULED;
        activeTasks.put(exec.getId(), exec);
        Future future = tasks.executor(exec.setup.category).fork(exec);
        future.onComplete(new CompletionHandler<Object>() {
            @Override
            public void onSuccess(@Nullable Object value) throws Exception {
                exec.state = ManagedTask.State.TERMINATED;
                exec.terminated = Instant.now();
            }

            @Override
            public void onFailure(Throwable throwable) throws Exception {
                exec.handle(throwable);
                exec.terminated = Instant.now();
                exec.state = ManagedTask.State.TERMINATED;
            }
        });
    }

    private void cleanupTasks() {
        if (!taskCleanupLimit.check()) {
            return;
        }
        Instant limit = Instant.now().minusSeconds(60);
        for (String taskId : Lists.newArrayList(activeTasks.keySet())) {
            ManagedTaskExecution exec = activeTasks.get(taskId);
            if (exec != null && exec.terminated != null && exec.terminated.isBefore(limit)) {
                activeTasks.remove(taskId);
            }
        }
    }

    /**
     * Tries to find a managed task with the given id.
     *
     * @param taskId the id of the task
     * @return the task with the given ID or <tt>null</tt> if either the task does not exist anymore or the user
     * has no permission to access this task
     */
    @Nullable
    public ManagedTask findTask(String taskId) {
        UserInfo user = UserContext.getCurrentUser();
        if (!user.isLoggedIn()) {
            return null;
        }
        cleanupTasks();
        ManagedTaskExecution task = activeTasks.get(taskId);
        if (task != null) {
            if (task.hasCurrentUserAccess()) {
                return task;
            }
        }

        return null;
    }

    /**
     * Returns a list of all managed tasks visible to the current user.
     *
     * @return a list of all tasks visible to the user. These are eitehr the ones started by the user or all tasks, if
     * the permission {@link #PERMISSION_ALL_TASKS} is present
     */
    public List<ManagedTask> getActiveTasks() {
        List<ManagedTask> result = Lists.newArrayList();
        UserInfo user = UserContext.getCurrentUser();
        if (!user.isLoggedIn()) {
            return result;
        }
        cleanupTasks();
        for (ManagedTaskExecution task : activeTasks.values()) {
            if (task.hasCurrentUserAccess()) {
                result.add(task);
            }
        }

        Collections.sort(result, Comparator.comparing(ManagedTask::getScheduled));

        return result;
    }
}
