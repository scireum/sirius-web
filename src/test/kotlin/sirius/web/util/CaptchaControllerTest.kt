/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.util

import org.altcha.altcha.v1.Altcha
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Json
import sirius.kernel.di.std.Part
import sirius.web.services.JSONStructuredOutput
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.Base64
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Tests the local ALTCHA integration contract used by [CaptchaController].
 */
@ExtendWith(SiriusExtension::class)
class CaptchaControllerTest {

    @Test
    fun `challenge serialization uses the legacy widget contract`() {
        val challenge = captchaController.createCaptchaChallenge(CAPTCHA_SECRET)
        val outputStream = ByteArrayOutputStream()
        val jsonOutput = JSONStructuredOutput(outputStream, StandardCharsets.UTF_8.name())

        jsonOutput.beginResult()
        captchaController.writeCaptchaChallenge(jsonOutput, challenge)
        jsonOutput.endResult()

        val json = Json.parseObject(outputStream.toString(StandardCharsets.UTF_8))
        assertEquals(challenge.algorithm(), json.get("algorithm").asString())
        assertEquals(challenge.challenge(), json.get("challenge").asString())
        assertEquals(challenge.maxnumber(), json.get("maxnumber").asLong())
        assertEquals(challenge.salt(), json.get("salt").asString())
        assertEquals(challenge.signature(), json.get("signature").asString())
    }

    @Test
    fun `valid widget payload is accepted`() {
        val challenge = captchaController.createCaptchaChallenge(CAPTCHA_SECRET)
        val payload = createWidgetPayload(challenge)

        assertTrue { captchaController.isValidCaptchaPayload(payload, CAPTCHA_SECRET) }
    }

    @Test
    fun `invalid widget payload is rejected`() {
        val challenge = captchaController.createCaptchaChallenge(CAPTCHA_SECRET)
        val payload = createWidgetPayload(challenge, signature = tamper(challenge.signature()))

        assertFalse { captchaController.isValidCaptchaPayload(payload, CAPTCHA_SECRET) }
        assertFalse { captchaController.isValidCaptchaPayload("", CAPTCHA_SECRET) }
    }

    private fun createWidgetPayload(
        challenge: Altcha.Challenge,
        signature: String = challenge.signature()
    ): String {
        val solution = Altcha.solveChallenge(
            challenge.challenge(),
            challenge.salt(),
            Altcha.Algorithm.fromString(challenge.algorithm()),
            challenge.maxnumber(),
            0
        )

        val payload = "{" +
                "\"algorithm\":\"${challenge.algorithm()}\"," +
                "\"challenge\":\"${challenge.challenge()}\"," +
                "\"number\":${solution.number()}," +
                "\"salt\":\"${challenge.salt()}\"," +
                "\"signature\":\"$signature\"" +
                "}"

        return Base64.getEncoder().encodeToString(payload.toByteArray(StandardCharsets.UTF_8))
    }

    private fun tamper(value: String): String {
        val replacement = if (value.last() == '0') '1' else '0'
        return value.dropLast(1) + replacement
    }

    companion object {
        private const val CAPTCHA_SECRET = "superSecureCaptchaSecret"

        @Part
        @JvmStatic
        private lateinit var captchaController: CaptchaController
    }
}
