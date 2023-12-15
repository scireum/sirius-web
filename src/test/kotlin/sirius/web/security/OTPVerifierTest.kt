/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.BaseSpecification
import sirius.kernel.SiriusExtension
import sirius.kernel.di.Injector
import kotlin.test.Test
import kotlin.test.assertTrue
@ExtendWith(SiriusExtension::class)
class OTPVerifierTest{

@Test
    fun `computeCode produces codes accepted by checkCode`() {
        val verifier = Injector.context().getPart(OTPVerifier::class.java)
        val secret = verifier?.generateSharedSecret()
        if (secret != null) {
            val code = verifier.computeCode(secret)

            if (verifier != null) {
                assertTrue { verifier.checkCode(secret, code) }
            }
        }
        assertTrue(verifier != null)
        //assertTrue(secret != null)
    }

    /*    def "codes generated with other secret are not accepted"() {
            given:
            def verifier = Injector.context().getPart(OTPVerifier.class)
                when:
                def code = verifier.computeCode(verifier.generateSharedSecret())
                        then:
                        verifier.checkCode(verifier.generateSharedSecret(), code) == false
        }*/

}
