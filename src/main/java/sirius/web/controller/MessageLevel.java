package sirius.web.controller;

/**
 * Declares the severity of a message and provides a convenient way to determine a proper Bootstrap CSS class to render
 * the message.
 */
public enum MessageLevel {

    /**
     * Declares a message as success.
     */
    SUCCESS("alert-success"),

    /**
     * Declares a message as information.
     */
    INFO("alert-info"),

    /**
     * Declares a message as warning.
     */
    WARNING("alert-warning"),

    /**
     * Declares a message as error.
     */
    PROBLEM("alert-danger");

    private String cssClass;

    MessageLevel(String cssClass) {
        this.cssClass = cssClass;
    }

    /**
     * Returns the Bootstrap CSS class used to render the message.
     *
     * @return the name of the css class used to render the message
     */
    public String getCssClass() {
        return cssClass;
    }
}