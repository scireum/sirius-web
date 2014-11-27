/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health.console;

import sirius.kernel.async.CallContext;
import sirius.kernel.di.std.Register;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;
import java.util.Map;

/**
 * Console command which reports all running threads.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
@Register(name = "threads")
public class ThreadsCommand implements Command {

    private ThreadMXBean t = ManagementFactory.getThreadMXBean();

    @Override
    public void execute(Output output, String... params) throws Exception {
        if (params.length == 1) {
            for (Map.Entry<Thread, StackTraceElement[]> thread : Thread.getAllStackTraces().entrySet()) {
                if ("all".equalsIgnoreCase(params[0]) || thread.getKey()
                                                               .getName()
                                                               .toLowerCase()
                                                               .contains(params[0].toLowerCase())) {
                    output.blankLine();
                    output.line(thread.getKey().getName());
                    output.separator();
                    for (StackTraceElement e : thread.getValue()) {
                        output.apply("%-60s %19s",
                                     e.getClassName() + "." + e.getMethodName(),
                                     e.getFileName() + ":" + e.getLineNumber());
                    }
                    output.separator();
                    Map<String, String> mdc = CallContext.getMDC(thread.getKey().getId());
                    if (!mdc.isEmpty()) {
                        output.line("Mapped Diagnostic Context");
                        output.separator();
                        for (Map.Entry<String, String> e : mdc.entrySet()) {
                            output.apply("%-20s %59s", e.getKey(), e.getValue());
                        }
                    }
                    output.blankLine();
                }
            }
        } else {
            output.apply("%-15s %10s %53s", "STATE", "ID", "NAME");
            output.separator();
            for (ThreadInfo info : t.dumpAllThreads(false, false)) {
                output.apply("%-15s %10s %53s", info.getThreadState().name(), info.getThreadId(), info.getThreadName());
            }
            output.separator();
        }
    }

    @Override
    public String getName() {
        return "threads";
    }

    @Override
    public String getDescription() {
        return "Reports a list of all threads or creates a stack trace of a given tread or all threads (using 'all' as param)";
    }
}
