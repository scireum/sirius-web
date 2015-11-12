/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.jobs;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import sirius.kernel.async.CompletionHandler;
import sirius.kernel.async.Future;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.commons.Strings;
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
 * Created by aha on 30.10.15.
 */
@Register(classes = Jobs.class)
public class Jobs {

    public static final Log LOG = Log.get("jobs");

    private Map<String, ManagedTaskExecution> activeTasks = Collections.synchronizedMap(Maps.newLinkedHashMap());
    private RateLimit taskCleanupLimit = RateLimit.timeInterval(10, TimeUnit.SECONDS);

    @Part
    private Tasks tasks;

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

    @Nullable
    public ManagedTask findTask(String taskId) {
        UserInfo user = UserContext.getCurrentUser();
        if (!user.isLoggedIn()) {
            return null;
        }
        cleanupTasks();
        ManagedTaskExecution task = activeTasks.get(taskId);
        if (task != null) {
            if (user.hasPermission(PERMISSION_ALL_TASKS) || Strings.areEqual(user.getUserId(), task.getUserId())) {
                return task;
            }
        }

        return null;
    }

    public List<ManagedTask> getActiveTasks() {
        List<ManagedTask> result = Lists.newArrayList();
        UserInfo user = UserContext.getCurrentUser();
        if (!user.isLoggedIn()) {
            return result;
        }
        cleanupTasks();
        for(ManagedTaskExecution task : activeTasks.values()) {
            if (user.hasPermission(PERMISSION_ALL_TASKS) || Strings.areEqual(user.getUserId(), task.getUserId())) {
                result.add(task);
            }
        }

        Collections.sort(result, Comparator.comparing(ManagedTaskExecution::getScheduled));
    }
}
