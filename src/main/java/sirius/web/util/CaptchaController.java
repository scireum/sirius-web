/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import org.altcha.altcha.v1.Altcha;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.web.controller.BasicController;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.services.InternalService;
import sirius.web.services.JSONStructuredOutput;

/**
 * Provides captcha-related functionality for bot and spam protection in forms. This is advisable for public contact or
 * registration forms.
 * <p>
 * To use the captcha, a challenge is requested via {@link #captchaChallenge(WebContext, JSONStructuredOutput)} by
 * <tt>t:captcha</tt>. To verify the solution, {@link #verifyCaptcha(WebContext)} must be called in the form-submit
 * route to make the protection work properly.
 * <p>
 * This controller intentionally uses the ALTCHA v1 widget payload contract, which submits the solution in the
 * <tt>altcha</tt> form field and signs challenges using the configured <tt>http.captcha.secret</tt>.
 * <p>
 * For more information, see <a href="https://github.com/altcha-org/altcha">Altcha on GitHub</a>.
 */
@Register(classes = {CaptchaController.class, Controller.class})
public class CaptchaController extends BasicController {

    private static final String NLS_CAPTCHA_FAILED = "CaptchaController.captchaFailed";

    /**
     * Contains the HMAC secret used to sign and verify captcha challenges.
     */
    @ConfigValue("http.captcha.secret")
    private static String captchaSecret;

    /**
     * Provides a captcha challenge for form submissions.
     * <p>
     * Called by <tt>t:captcha</tt> to obtain a new challenge.
     *
     * @param webContext the current request
     * @param output     JSON output for writing the challenge to the request
     */
    @InternalService
    @Routed("/captcha-challenge")
    public void captchaChallenge(WebContext webContext, JSONStructuredOutput output) {
        try {
            writeCaptchaChallenge(output, createCaptchaChallenge(captchaSecret));
        } catch (Exception _) {
            throw Exceptions.createHandled().withNLSKey(NLS_CAPTCHA_FAILED).handle();
        }
    }

    /**
     * Creates a new ALTCHA v1 challenge using the given secret.
     *
     * @param secret the HMAC secret used to sign the challenge
     * @return the generated challenge to send to the client
     * @throws Exception in case challenge creation fails
     */
    Altcha.Challenge createCaptchaChallenge(String secret) throws Exception {
        Altcha.ChallengeOptions options = new Altcha.ChallengeOptions();
        options.hmacKey = secret;

        return Altcha.createChallenge(options);
    }

    /**
     * Writes the challenge properties expected by the current ALTCHA widget integration.
     *
     * @param output    JSON output for writing the challenge properties
     * @param challenge the challenge to serialize
     */
    void writeCaptchaChallenge(JSONStructuredOutput output, Altcha.Challenge challenge) {
        output.property("algorithm", challenge.algorithm());
        output.property("challenge", challenge.challenge());
        output.property("maxnumber", challenge.maxnumber());
        output.property("salt", challenge.salt());
        output.property("signature", challenge.signature());
    }

    /**
     * Verifies the captcha solution contained in the given context.
     * <p>
     * This method should be called in form-submit routes to verify the captcha solution.
     *
     * @param webContext the web context containing the captcha solution
     */
    public void verifyCaptcha(WebContext webContext) {
        try {
            String payload = webContext.get("altcha").asString();
            if (!isValidCaptchaPayload(payload, captchaSecret)) {
                throw Exceptions.createHandled().withNLSKey(NLS_CAPTCHA_FAILED).handle();
            }
        } catch (Exception _) {
            throw Exceptions.createHandled().withNLSKey(NLS_CAPTCHA_FAILED).handle();
        }
    }

    /**
     * Verifies a widget-submitted ALTCHA v1 payload using the given secret.
     *
     * @param payload the base64 encoded ALTCHA payload submitted by the widget
     * @param secret  the HMAC secret used to verify the payload
     * @return <tt>true</tt> if the payload is present and valid, <tt>false</tt> otherwise
     */
    boolean isValidCaptchaPayload(String payload, String secret) {
        if (Strings.isEmpty(payload)) {
            return false;
        }

        try {
            return Altcha.verifySolution(payload, secret, true);
        } catch (Exception _) {
            return false;
        }
    }
}
