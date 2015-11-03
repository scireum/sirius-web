/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.jobs;

import java.util.List;

/**
 * Created by aha on 31.10.15.
 */
public interface ManagedTask {

    String getId();

    String getName();

    String getState();

    List<TaskLogEntry> getLastLogs();

    void cancel();
}
