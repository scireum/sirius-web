/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.tasks;

import sirius.kernel.nls.NLS;

import java.time.Instant;

/**
 * Represents a log entry for a managed task.
 */
@Deprecated
public class TaskLogEntry {

    /**
     * Represents the type of an entry
     */
    public enum LogType {
        TRACE, NORMAL, SUCCESS, WARN, ERROR
    }

    private Instant tod;
    private String message;
    private LogType type;

    /**
     * Creates a new entry with the given message and type
     *
     * @param message the message to log
     * @param type    the type / level to log
     */
    public TaskLogEntry(String message, LogType type) {
        this.tod = Instant.now();
        this.message = message;
        this.type = type;
    }

    /**
     * Returns the timestamp (time of day) when the message was logged.
     *
     * @return the timestamp when the log entry was written
     */
    public Instant getTod() {
        return tod;
    }

    /**
     * Returns the message of the log entry.
     *
     * @return the log entry
     */
    public String getMessage() {
        return message;
    }

    /**
     * Returns the type of the log entry.
     *
     * @return the type of the log entry
     */
    public LogType getType() {
        return type;
    }

    @Override
    public String toString() {
        return NLS.toUserString(tod) + " " + type.toString() + ": " + NLS.toUserString(message);
    }
}
