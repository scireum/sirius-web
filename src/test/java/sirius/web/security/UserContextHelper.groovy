/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.web.controller.Message
import sirius.web.controller.MessageLevel
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
    static UserContext getUserContext(TestResponse response) {
        return response.getCallContext().get(UserContext.class)
    }

    /**
     * Expects that no messages were created at all.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectNoMessages(TestResponse response) {
        assert getUserContext(response).getMessages().isEmpty()
    }

    /**
     * Expects that at least one message was created at all.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectMessage(TestResponse response) {
        assert !getUserContext(response).getMessages().isEmpty()
    }

    /**
     * Expects that no error messages where created at all.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectNoErrorMessages(TestResponse response) {
        expectNoMessagesOfType(response, MessageLevel.PROBLEM)
    }

    /**
     * Expects that at least one error message was created.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectErrorMessage(TestResponse response) {
        expectMessageOfType(response, MessageLevel.PROBLEM)
    }

    /**
     * Expects that at least one error message was created which contains the given text part.
     *
     * @param response the response to dissect
     * @param textPart the expected text part to be contained in the error message
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectErrorMessageContaining(TestResponse response, String textPart) {
        expectMessageOfTypeContaining(response, MessageLevel.PROBLEM, textPart)
    }

    /**
     * Expects that no warn messages where created at all.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectNoWarnMessages(TestResponse response) {
        expectNoMessagesOfType(response, MessageLevel.WARNING)
    }

    /**
     * Expects that at least one warn message was created.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectWarnMessage(TestResponse response) {
        expectMessageOfType(response, MessageLevel.WARNING)
    }

    /**
     * Expects that at least one warn message was created which contains the given text part.
     *
     * @param response the response to dissect
     * @param textPart the expected text part to be contained in the success message
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectWarnMessageContaining(TestResponse response, String textPart) {
        expectMessageOfTypeContaining(response, MessageLevel.WARNING, textPart)
    }

    /**
     * Expects that no success messages where created at all.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectNoInfoMessages(TestResponse response) {
        expectNoMessagesOfType(response, MessageLevel.INFO)
    }

    /**
     * Expects that at least one info message was created.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectInfoMessage(TestResponse response) {
        expectMessageOfType(response, MessageLevel.INFO)
    }

    /**
     * Expects that at least one info message was created which contains the given text part.
     *
     * @param response the response to dissect
     * @param textPart the expected text part to be contained in the success message
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectInfoMessageContaining(TestResponse response, String textPart) {
        expectMessageOfTypeContaining(response, MessageLevel.INFO, textPart)
    }


    /**
     * Expects that no success messages where created at all.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectNoSuccessMessages(TestResponse response) {
        expectNoMessagesOfType(response, MessageLevel.SUCCESS)
    }

    /**
     * Expects that at least one success message was created.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectSuccessMessage(TestResponse response) {
        expectMessageOfType(response, MessageLevel.SUCCESS)
    }

    /**
     * Expects that at least one success message was created which contains the given text part.
     *
     * @param response the response to dissect
     * @param textPart the expected text part to be contained in the success message
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectSuccessMessageContaining(TestResponse response, String textPart) {
        expectMessageOfTypeContaining(response, MessageLevel.SUCCESS, textPart)
    }

    /**
     * Expects that there are no field errors.
     *
     * @param response the response to dissect
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectNoFieldErrors(TestResponse response) {
        def ctx = getUserContext(response)
        assert ctx.getFieldErrors().isEmpty(): ctx.getFieldErrors()
                                                  .keySet()
                                                  .collectEntries({ field -> [field, ctx.getFieldErrorMessage(field)] })
    }

    /**
     * Expects that there is a field error for the given field.
     *
     * @param response the response to dissect
     * @param field the name of the field that is expected to have a field error
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    static void expectFieldError(TestResponse response, String field) {
        def ctx = getUserContext(response)
        assert ctx.getFieldErrors(field).containsKey(field)
    }

    /**
     * Expects that no messages of the given type were created at all.
     *
     * @param response the response to dissect
     * @param type the message type
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    private static void expectNoMessagesOfType(TestResponse response, MessageLevel type) {
        def ctx = getUserContext(response)
        assert ctx.getMessages().every { msg -> msg.getType() != type }
    }

    /**
     * Expects that at least one message of the given type was created at all.
     *
     * @param response the response to dissect
     * @param type the message type
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    private static void expectMessageOfType(TestResponse response, MessageLevel type) {
        assert getUserContext(response).getMessages().any { msg -> msg.getType() == type }
    }

    /**
     * Expects that at least one message of the given type that contains the given text was created at all.
     *
     * @param response the response to dissect
     * @param type the message type
     * @param textPart the text that the message must contain
     * @return <tt>true</tt> if the assertion is fulfilled, <tt>false</tt> otherwise
     */
    private static void expectMessageOfTypeContaining(TestResponse response, MessageLevel type, String textPart) {
        assert getUserContext(response)
                .getMessages()
                .any { msg -> msg.getType() == type && msg.getMessage().contains(textPart) }
    }
}
