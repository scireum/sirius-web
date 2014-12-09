/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.kernel.BaseSpecification
import sirius.kernel.di.Injector

class OTPVerifierSpec extends BaseSpecification {

    def "computeCode produces codes accepted by checkCode"() {
        given:
        def verifier = Injector.context().getPart(OTPVerifier.class);
        when:
        def secret = verifier.generateSharedSecret();
        and:
        def code = verifier.computeCode(secret);
        then:
        verifier.checkCode(secret, code)
    }

    def "codes generated with other secret are not accepted"() {
        given:
        def verifier = Injector.context().getPart(OTPVerifier.class);
        when:
        def code = verifier.computeCode(verifier.generateSharedSecret());
        then:
        verifier.checkCode(verifier.generateSharedSecret(), code) == false
    }

}
