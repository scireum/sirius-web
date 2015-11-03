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
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Log;

import javax.annotation.Nullable;
import java.time.Instant;
import java.util.Collections;
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
        exec.scheduled = Instant.now();
        cleanupTasks();
        activeTasks.put(exec.getId(), exec);
        Future future = tasks.executor(exec.setup.category).fork(exec);
        future.onComplete(new CompletionHandler<Object>() {
            @Override
            public void onSuccess(@Nullable Object value) throws Exception {
                exec.terminated = Instant.now();
            }

            @Override
            public void onFailure(Throwable throwable) throws Exception {
                exec.handle(throwable);
                exec.erroneous = true;
                exec.terminated = Instant.now();
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
            if (exec != null && exec.terminated.isBefore(limit)) {
                activeTasks.remove(taskId);
            }
        }
    }

    @Nullable
    public ManagedTask findTask(String taskId) {
        cleanupTasks();
        return activeTasks.get(taskId);
    }
}
