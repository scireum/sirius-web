/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import com.google.common.io.BaseEncoding;
import org.altcha.altcha.Altcha;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.web.controller.BasicController;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.services.InternalService;
import sirius.web.services.JSONStructuredOutput;

import java.security.SecureRandom;

/**
 * Provides captcha related functionality for bot/spam-protection in forms.
 * This may be helpful for public forms like contact or registration forms.
 * For more information, see <a href="https://github.com/altcha-org/altcha">Altcha on GitHub</a>.
 * To use the captcha, a challenge is requested via {@link #captchaChallenge(WebContext, JSONStructuredOutput)} by the
 * Taglib t:captcha. To verify the solution, {@link #verifyCaptcha(WebContext)} must be
 * implemented in the form-submit Route to make protection work properly.
 *
 */
@Register(classes = {CaptchaController.class, Controller.class})
public class CaptchaController extends BasicController {

    private static final String NLS_CAPTCHA_FAILED = "CaptchaController.captchaFailed";

    private static final String CAPTCHA_CHALLENGE_HMAC_KEY =
            BaseEncoding.base32().encode(new SecureRandom().generateSeed(8));

    /**
     * Provides a captcha challenge for form submissions.
     * Called by the Taglib t:captcha to obtain a new challenge.
     *
     * @param webContext the current request
     * @param output     JSON output for writing the challenge to the request
     */
    @InternalService
    @Routed("/captcha-challenge")
    public void captchaChallenge(WebContext webContext, JSONStructuredOutput output) {
        try {
            Altcha.ChallengeOptions options = new Altcha.ChallengeOptions();
            options.hmacKey = CAPTCHA_CHALLENGE_HMAC_KEY;

            Altcha.Challenge challenge = Altcha.createChallenge(options);
            output.property("algorithm", challenge.algorithm);
            output.property("challenge", challenge.challenge);
            output.property("maxnumber", challenge.maxnumber);
            output.property("salt", challenge.salt);
            output.property("signature", challenge.signature);
        } catch (Exception _) {
            throw Exceptions.createHandled().withNLSKey(NLS_CAPTCHA_FAILED).handle();
        }
    }

    /**
     * Verifies the captcha solution contained in the given context.
     * This method should be called in form-submit routes to verify the captcha solution.
     *
     * @param webContext the web context containing the captcha solution
     */
    public void verifyCaptcha(WebContext webContext) {
        try {
            String payload = webContext.get("altcha").asString();
            if (Strings.isEmpty(payload)) {
                throw Exceptions.createHandled().withNLSKey(NLS_CAPTCHA_FAILED).handle();
            }

            boolean isValid = Altcha.verifySolution(payload, CAPTCHA_CHALLENGE_HMAC_KEY, true);
            if (!isValid) {
                throw Exceptions.createHandled().withNLSKey(NLS_CAPTCHA_FAILED).handle();
            }
        } catch (Exception _) {
            throw Exceptions.createHandled().withNLSKey(NLS_CAPTCHA_FAILED).handle();
        }
    }
}
