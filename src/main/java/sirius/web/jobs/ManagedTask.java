/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.jobs;

import java.time.Instant;
import java.util.List;

/**
 * Created by aha on 31.10.15.
 */
public interface ManagedTask {

    enum State {
        SCHEDULED, RUNNING, WARNINGS, TERMINATED
    }

    String getId();

    String getUserId();

    String getUsername();

    Instant getScheduled();

    Instant getStarted();

    Instant getTerminated();

    String getName();

    State getState();

    List<TaskLogEntry> getLastLogs();

    void cancel();
}
