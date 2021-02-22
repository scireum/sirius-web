package sirius.web.controller;

/**
 * Declares the severity of a message and provides a convenient way to determine a proper Bootstrap CSS class to render
 * the message.
 */
public enum MessageLevel {

    /**
     * Declares a message as success.
     */
    SUCCESS("alert-success", "green"),

    /**
     * Declares a message as information.
     */
    INFO("alert-info", "blue"),

    /**
     * Declares a message as warning.
     */
    WARNING("alert-warning", "yellow"),

    /**
     * Declares a message as error.
     */
    PROBLEM("alert-danger", "red");

    private final String cssClass;
    private final String color;

    MessageLevel(String cssClass, String color) {
        this.cssClass = cssClass;
        this.color = color;
    }

    /**
     * Returns the Bootstrap CSS class used to render the message.
     *
     * @return the name of the css class used to render the message
     * @deprecated Used by Wonderger. Tycho will use getColor.
     */
    @Deprecated
    public String getCssClass() {
        return cssClass;
    }

    /**
     * Returns the color used to represent this message level.
     *
     * @return the name of the color used for this message level
     */
    public String getColor() {
        return color;
    }
}
