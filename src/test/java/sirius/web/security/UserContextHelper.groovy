/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.web.controller.Message
import sirius.web.http.TestResponse

/**
 * Provides boiler plate methods to assert a certain state in the {@link sirius.web.security.UserContext}.
 * <p>
 * This can be used to verify that no error messages where generated when using a template. This is probably used
 * in combination with {@link sirius.web.http.TestRequest}.
 */
class UserContextHelper {

    private UserContextHelper() {
    }

    /**
     * Extracts the {@link sirius.web.security.UserContext} which was active when the response was built.
     *
     * @param response the response to dissect
     * @return the user context which was able while creating the response
     */
    public static UserContext getUserContext(TestResponse response) {
        return response.getCallContext().get(UserContext.class)
    }

    /**
     * Expects that no messages were created at all
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static void expectNoMessages(TestResponse response) {
        assert getUserContext(response).getMessages().isEmpty()
    }

    /**
     * Expects that no error messages where created at all
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static void expectNoErrorMessages(TestResponse response) {
        assert !getUserContext(response).getMessages().any { msg -> msg.getType() == Message.ERROR }
    }

    /**
     * Expects that at least one error message was created
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static void expectErrorMessage(TestResponse response) {
        assert getUserContext(response).getMessages().any { msg -> msg.getType() == Message.ERROR }
    }

    /**
     * Expects that at least one error message was created which contains the given text part
     *
     * @param textPart the expected text part to be contained in the error message
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static void expectErrorMessageContaining(TestResponse response, String textPart) {
        assert getUserContext(response).getMessages().any { msg -> msg.getType() == Message.ERROR && msg.getMessage().contains(textPart) }
    }

    /**
     * Expects that at least one success (info) message was created
     *
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static void expectSuccessMessage(TestResponse response) {
        assert getUserContext(response).getMessages().any { msg -> msg.getType() == Message.INFO }
    }

    /**
     * Expects that at least one success (info) message was created which contains the given text part
     *
     * @param textPart the expected text part to be contained in the success message
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    public static void expectSuccessMessageContaining(TestResponse response, String textPart) {
        assert getUserContext(response).getMessages().any { msg -> msg.getType() == Message.INFO && msg.getMessage().contains(textPart) }
    }
}
