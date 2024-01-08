/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.di.Injector
import kotlin.test.assertFalse
import kotlin.test.assertTrue

@ExtendWith(SiriusExtension::class)
class OTPVerifierTest {

    @Test
    fun `computeCode produces codes accepted by checkCode`() {

        val verifier = Injector.context().getPart(OTPVerifier::class.java)

        val secret = verifier!!.generateSharedSecret()

        val code = verifier.computeCode(secret)

        assertTrue { verifier.checkCode(secret, code) }
    }

    @Test
    fun `codes generated with other secret are not accepted`() {

        val verifier = Injector.context().getPart(OTPVerifier::class.java)

        val code = verifier!!.computeCode(verifier.generateSharedSecret())

        assertFalse { verifier.checkCode(verifier.generateSharedSecret(), code) }
    }

}
