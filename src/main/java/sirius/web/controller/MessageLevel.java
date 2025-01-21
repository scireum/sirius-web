package sirius.web.controller;

import sirius.pasta.noodle.sandbox.NoodleSandbox;

/**
 * Declares the severity of a message and provides a convenient way to determine a proper Bootstrap CSS class to render
 * the message.
 */
public enum MessageLevel {

    /**
     * Declares a message as success.
     */
    SUCCESS("alert-success", "sci-message-banner-success", "green"),

    /**
     * Declares a message as information.
     */
    INFO("alert-info", "sci-message-banner-info", "blue"),

    /**
     * Declares a message as warning.
     */
    WARNING("alert-warning", "sci-message-banner-warning", "yellow"),

    /**
     * Declares a message as error.
     */
    PROBLEM("alert-danger", "sci-message-banner-error", "red");

    private final String cssClass;
    private final String designSystemClass;
    private final String color;

    MessageLevel(String cssClass, String designSystemClass, String color) {
        this.cssClass = cssClass;
        this.designSystemClass = designSystemClass;
        this.color = color;
    }

    /**
     * Returns the Bootstrap CSS class used to render the message.
     *
     * @return the name of the css class used to render the message
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getCssClass() {
        return cssClass;
    }

    /**
     * Returns the scireum Design System CSS class used to render the message.
     *
     * @return the name of the css class used to render the message
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getDesignSystemClass() {
        return designSystemClass;
    }

    /**
     * Returns the color used to represent this message level.
     *
     * @return the name of the color used for this message level
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getColor() {
        return color;
    }
}
