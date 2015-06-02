/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.web.controller.Message;

/**
 * Provides boiler plate methods to assert a certain state in the {@link sirius.web.security.UserContext}.
 * <p>
 * This can be used to verify that no error messages where generated when using a template. This is probably used
 * in combination with {@link sirius.web.http.TestRequest}.
 */
public class UserContextHelper {

    private UserContextHelper() {
    }

    /**
     * Expects that no messages where created at all
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static boolean expectNoMessages() {
        return UserContext.get().getMessages().isEmpty();
    }

    /**
     * Expects that no error messages where created at all
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static boolean expectNoErrorMessages() {
        for (Message msg : UserContext.get().getMessages()) {
            if (msg.getType() == Message.ERROR) {
                return false;
            }
        }
        return true;
    }

    /**
     * Expects that at least one error message was created
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static boolean expectErrorMessage() {
        for (Message msg : UserContext.get().getMessages()) {
            if (msg.getType() == Message.ERROR) {
                return true;
            }
        }
        return false;
    }

    /**
     * Expects that at least one error message was created which contains the given text part
     *
     * @param textPart the expected text part to be contained in the error message
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static boolean expectErrorMessageContaining(String textPart) {
        for (Message msg : UserContext.get().getMessages()) {
            if (msg.getType() == Message.ERROR && msg.getMessage().contains(textPart)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Expects that at least one success (info) message was created
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static boolean expectSuccessMessage() {
        for (Message msg : UserContext.get().getMessages()) {
            if (msg.getType() == Message.INFO) {
                return true;
            }
        }
        return false;
    }

    /**
     * Expects that at least one success (info) message was created which contains the given text part
     *
     * @param textPart the expected text part to be contained in the success message
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static boolean expectSuccessMessageContaining(String textPart) {
        for (Message msg : UserContext.get().getMessages()) {
            if (msg.getType() == Message.INFO && msg.getMessage().contains(textPart)) {
                return true;
            }
        }
        return false;
    }
}
