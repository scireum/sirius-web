/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.jobs;

import java.time.Instant;

/**
 * Created by aha on 02.11.15.
 */
public class TaskLogEntry {

    public enum LogType {
        TRACE, NORMAL, SUCCESS, WARN, ERROR
    }

    private Instant tod;
    private String message;
    private LogType type;

    public TaskLogEntry(String message, LogType type) {
        this.tod = Instant.now();
        this.message = message;
        this.type = type;
    }

    public Instant getTod() {
        return tod;
    }

    public String getMessage() {
        return message;
    }

    public LogType getType() {
        return type;
    }
}
