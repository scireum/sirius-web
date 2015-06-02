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
import sirius.kernel.async.Operation;
import sirius.kernel.di.std.Register;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Console command which reports statistics for all known executors.
 */
@Register
public class AsyncInfoCommand implements Command {

    @Override
    public void execute(Output output, String... params) throws Exception {
        output.apply("%-20s %8s %8s %8s %12s %8s %8s",
                     "POOL",
                     "ACTIVE",
                     "QUEUED",
                     "TOTAL",
                     "DURATION",
                     "BLOCKED",
                     "DROPPED");
        output.separator();
        for (AsyncExecutor exec : Async.getExecutors()) {
            output.apply("%-20s %8d %8d %8d %12.1f %8d %8d",
                         exec.getCategory(),
                         exec.getActiveCount(),
                         exec.getQueue().size(),
                         exec.getExecuted(),
                         exec.getAverageDuration(),
                         exec.getBlocked(),
                         exec.getDropped());
        }
        output.separator();
        output.blankLine();
        output.apply("Background Queues");
        output.separator();
        for (String worker : Async.getBackgroundWorkers()) {
            output.line(worker);
        }
        output.separator();
        List<Operation> ops = Operation.getActiveOperations();
        if (!ops.isEmpty()) {
            output.blankLine();
            output.apply("Active Operations");
            output.separator();
            for (Operation op : ops) {
                output.line(op.toString());
            }
            output.separator();
        }
    }

    @Override
    @Nonnull
    public String getName() {
        return "async";
    }

    @Override
    public String getDescription() {
        return "Reports the status of the task queueing system";
    }
}
