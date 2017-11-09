/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.web.security.UserContext;

/**
 * Used by the {@link sirius.web.security.UserContext} to collect error or success messages.
 */
public class Message {

    /**
     * Declares a message as information.
     */
    public static final String INFO = "alert-info";

    /**
     * Declares a message as warning.
     */
    public static final String WARN = "alert-warning";

    /**
     * Declares a message as error.
     */
    public static final String ERROR = "alert-danger";

    private String messageText;
    private String details;
    private String type;
    private String action;
    private String actionLabel;
    private boolean actionJavascript;

    /**
     * Creates a new message with the given content, details info and message type.
     *
     * @param message the message to show to the user
     * @param details the details for this message
     * @param type    the type for the message
     */
    public Message(String message, String details, String type) {
        this.messageText = message;
        this.details = details;
        this.type = type;
    }

    /**
     * Returns the message for the user.
     *
     * @return the message to show
     */
    public String getMessage() {
        return messageText;
    }

    /**
     * Returns detailed infos for this message
     *
     * @return the details to show to the user
     */
    public String getDetails() {
        return details;
    }

    /**
     * Returns the type of the message
     *
     * @return the type (one of {@link #INFO}, {@link #WARN}, {@link #ERROR})
     */
    public String getType() {
        return type;
    }

    /**
     * Returns the action associated with the message
     *
     * @return a URL which will be invoked when the user clicks on the message
     */
    public String getAction() {
        return action;
    }

    /**
     * Returns the label to be used for the action
     *
     * @return the label for the action which is shown to the user
     */
    public String getActionLabel() {
        return actionLabel;
    }

    /**
     * Determines if the action is an URL or a piece of JavaScript
     *
     * @return <tt>true</tt> if it is JS, <tt>false</tt> otherwise
     */
    public boolean isActionJavascript() {
        return actionJavascript;
    }

    /**
     * Adds an action as URL to the message
     *
     * @param link  the URL to invoke if the user clicks on the message
     * @param label the label to use for the given action
     * @return the message itself for further use
     */
    public Message withAction(String link, String label) {
        this.action = link;
        this.actionLabel = label;
        return this;
    }

    /**
     * Adds an action as URL to the message
     *
     * @param codeFragment the URL to invoke if the user clicks on the message
     * @param label        the label to use for the given action
     * @return the message itself for further use
     */
    public Message withJavascriptAction(String codeFragment, String label) {
        this.action = codeFragment;
        this.actionLabel = label;
        this.actionJavascript = true;
        return this;
    }

    @Override
    public String toString() {
        return messageText;
    }

    /**
     * Factory method to create a info message
     *
     * @param message the message content
     * @return a new message with the given content and INFO as type
     */
    public static Message info(String message) {
        return new Message(message, null, INFO);
    }

    /**
     * Factory method to create a warning as message
     *
     * @param message the message content
     * @return a new message with the given content and WARN as type
     */
    public static Message warn(String message) {
        return new Message(message, null, WARN);
    }

    /**
     * Factory method to create an error message
     *
     * @param message the message content
     * @return a new message with the given content and ERROR as type
     */
    public static Message error(String message) {
        return new Message(message, null, ERROR);
    }

    /**
     * Factory method to create an error message
     *
     * @param t the exception containing the error message
     * @return a new message with the given content and ERROR as type
     */
    public static Message error(Throwable t) {
        if (t instanceof HandledException) {
            return error(t.getMessage());
        }
        return new Message(Exceptions.handle(UserContext.LOG, t).getMessage(), null, ERROR);
    }
}
