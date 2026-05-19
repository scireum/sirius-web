/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.web.controller.MessageLevel
import sirius.web.http.TestResponse
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Provides boiler plate methods to assert a certain state in the [UserContext].
 *
 * This can be used to verify that no error messages where generated when using a template. This is probably used
 * in combination with [sirius.web.http.TestRequest].
 */
object UserContextHelper {

    /**
     * Extracts the [UserContext] which was active when the response was built.
     *
     * @param response the response to dissect
     * @return the user context which was able while creating the response
     */
    @JvmStatic
    fun getUserContext(response: TestResponse): UserContext {
        return response.callContext.getOrCreateSubContext(UserContext::class.java)
    }

    /**
     * Expects that no messages were created at all.
     */
    @JvmStatic
    fun expectNoMessages(response: TestResponse) {
        assertTrue { getUserContext(response).messages.isEmpty() }
    }

    /**
     * Expects that at least one message was created at all.
     */
    @JvmStatic
    fun expectAnyMessage(response: TestResponse) {
        assertFalse { getUserContext(response).messages.isEmpty() }
    }

    /**
     * Expects that no error messages where created at all.
     */
    @JvmStatic
    fun expectNoErrorMessages(response: TestResponse) {
        expectNoMessagesOfType(response, MessageLevel.PROBLEM)
    }

    /**
     * Expects that at least one error message was created.
     */
    @JvmStatic
    fun expectErrorMessage(response: TestResponse) {
        expectMessageOfType(response, MessageLevel.PROBLEM)
    }

    /**
     * Expects that at least one error message was created which contains the given text part.
     */
    @JvmStatic
    fun expectErrorMessageContaining(response: TestResponse, textPart: String) {
        expectMessageOfTypeContaining(response, MessageLevel.PROBLEM, textPart)
    }

    /**
     * Expects that no warn messages where created at all.
     */
    @JvmStatic
    fun expectNoWarnMessages(response: TestResponse) {
        expectNoMessagesOfType(response, MessageLevel.WARNING)
    }

    /**
     * Expects that at least one warn message was created.
     */
    @JvmStatic
    fun expectWarnMessage(response: TestResponse) {
        expectMessageOfType(response, MessageLevel.WARNING)
    }

    /**
     * Expects that at least one warn message was created which contains the given text part.
     */
    @JvmStatic
    fun expectWarnMessageContaining(response: TestResponse, textPart: String) {
        expectMessageOfTypeContaining(response, MessageLevel.WARNING, textPart)
    }

    /**
     * Expects that no info messages where created at all.
     */
    @JvmStatic
    fun expectNoInfoMessages(response: TestResponse) {
        expectNoMessagesOfType(response, MessageLevel.INFO)
    }

    /**
     * Expects that at least one info message was created.
     */
    @JvmStatic
    fun expectInfoMessage(response: TestResponse) {
        expectMessageOfType(response, MessageLevel.INFO)
    }

    /**
     * Expects that at least one info message was created which contains the given text part.
     */
    @JvmStatic
    fun expectInfoMessageContaining(response: TestResponse, textPart: String) {
        expectMessageOfTypeContaining(response, MessageLevel.INFO, textPart)
    }

    /**
     * Expects that no success messages where created at all.
     */
    @JvmStatic
    fun expectNoSuccessMessages(response: TestResponse) {
        expectNoMessagesOfType(response, MessageLevel.SUCCESS)
    }

    /**
     * Expects that at least one success message was created.
     */
    @JvmStatic
    fun expectSuccessMessage(response: TestResponse) {
        expectMessageOfType(response, MessageLevel.SUCCESS)
    }

    /**
     * Expects that at least one success message was created which contains the given text part.
     */
    @JvmStatic
    fun expectSuccessMessageContaining(response: TestResponse, textPart: String) {
        expectMessageOfTypeContaining(response, MessageLevel.SUCCESS, textPart)
    }

    /**
     * Expects that there are no field errors.
     */
    @JvmStatic
    fun expectNoFieldErrors(response: TestResponse) {
        val userContext = getUserContext(response)
        val fieldErrors = userContext.fieldErrors
        assertTrue(
            fieldErrors.isEmpty(),
            fieldErrors.keys.associateWith { userContext.getFieldErrorMessage(it) }.toString()
        )
    }

    /**
     * Expects that there is a field error for the given field.
     */
    @JvmStatic
    fun expectFieldError(response: TestResponse, field: String) {
        assertTrue { getUserContext(response).fieldErrors.containsKey(field) }
    }

    private fun expectNoMessagesOfType(response: TestResponse, type: MessageLevel) {
        assertTrue { getUserContext(response).messages.none { it.type == type } }
    }

    private fun expectMessageOfType(response: TestResponse, type: MessageLevel) {
        assertTrue { getUserContext(response).messages.any { it.type == type } }
    }

    private fun expectMessageOfTypeContaining(response: TestResponse, type: MessageLevel, textPart: String) {
        assertTrue {
            getUserContext(response).messages.any { it.type == type && it.html.contains(textPart) }
        }
    }
}
