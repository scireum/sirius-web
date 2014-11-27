/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health.console;

import sirius.kernel.async.Async;
import sirius.kernel.async.AsyncExecutor;
import sirius.kernel.di.std.Register;

/**
 * Console command which reports statistics for all known executors.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2013/08
 */
@Register(name = "async")
public class AsyncInfoCommand implements Command {

    @Override
    public void execute(Output output, String... params) throws Exception {
        output.apply("%-20s %8s %8s %16s %8s %8s", "POOL", "ACTIVE", "QUEUED", "TOTAL", "BLOCKED", "DROPPED");
        output.separator();
        for (AsyncExecutor exec : Async.getExecutors()) {
            output.apply("%-20s %8d %8d %16d %8d %8d",
                         exec.getCategory(),
                         exec.getActiveCount(),
                         exec.getQueue().size(),
                         exec.getCompletedTaskCount(),
                         exec.getBlocked(),
                         exec.getDropped());
        }
        output.separator();
    }

    @Override
    public String getName() {
        return "async";
    }

    @Override
    public String getDescription() {
        return "Reports the status of the task queueing system";
    }
}
