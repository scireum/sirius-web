/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.web.controller.Message;
import sirius.web.http.TestResponse;

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
    public static boolean expectNoMessages(TestResponse response) {
        return getUserContext(response).getMessages().isEmpty();
    }

    /**
     * Extracts the <tt>UserContext</tt> which was active when the response was built.
     *
     * @param response the response to dissect
     * @return the user context which was able while creating the response
     */
    public static UserContext getUserContext(TestResponse response) {
        return response.getCallContext().get(UserContext.class);
    }

    /**
     * Expects that no error messages where created at all
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static boolean expectNoErrorMessages(TestResponse response) {
        for (Message msg : getUserContext(response).getMessages()) {
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
    public static boolean expectErrorMessage(TestResponse response) {
        for (Message msg : getUserContext(response).getMessages()) {
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
    public static boolean expectErrorMessageContaining(TestResponse response, String textPart) {
        for (Message msg : getUserContext(response).getMessages()) {
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
    public static boolean expectSuccessMessage(TestResponse response) {
        for (Message msg : getUserContext(response).getMessages()) {
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
    public static boolean expectSuccessMessageContaining(TestResponse response,String textPart) {
        for (Message msg : getUserContext(response).getMessages()) {
            if (msg.getType() == Message.INFO && msg.getMessage().contains(textPart)) {
                return true;
            }
        }
        return false;
    }
}
